{-# LANGUAGE LambdaCase, OverloadedStrings, TypeFamilies, FlexibleContexts #-}
module Main where

import Prelude hiding (readFile, log)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteArray as BA
import Text.Printf
import System.IO (withFile, IOMode(..), SeekMode(..), hSeek)

import Data.Monoid
import qualified Data.SparseMaptrix as Mx
import Control.Applicative
import Options.Applicative
import Control.Monad.Reader

import Data.Torrent
import Data.Foldable (toList)
import qualified Crypto.Hash as CR
import Data.String
import qualified System.Directory as Dir
import qualified System.Directory.Tree as Dir
import System.FilePath (takeDirectory, takeBaseName, (</>))
import qualified Data.Text.Encoding as Txt
import qualified Data.Text as Txt
import qualified Data.Map as M
import Control.Monad.Logic
import Control.Monad.Plus
import Data.List (foldl1', foldl', nub)

import Data.Interval
import qualified Data.IntervalMap.Lazy as IM

import Debug.Trace

intInt :: Integer -> Integer -> Interval Integer
intInt a b = Finite a <=..< Finite b

type Hook = String

type MyMonad a = ReaderT Config IO a

data Config =
    Config
    { _copy           :: Bool
    , _verbosity      :: Int
    , _dryRun         :: Bool
    , _confidenceThreshold :: Float
    , _numSamples     :: Int
    , _torrentHook
    , _fileHook       :: Hook
    , _outDir
    , _fileDir        :: FilePath
    , _torrentFiles   :: [FilePath]
    } deriving (Show)

data FromFile =
    FromFile
    { _fromPath :: FilePath
    , _fromSize :: Integer
    } deriving (Show)

type ToFile = (Interval Integer, FilePath)
type ToFiles = IM.IntervalMap Integer ToFile

data Match = FilePath :-> FilePath
             deriving (Show)

cliOptions :: Parser Config
cliOptions = Config <$>
               switch (long "copy"
                    <> short 'C'
                    <> help "copy files instead of moving") <*>
               option auto (long "verbose"
                         <> short 'v'
                         <> value 1) <*>
               switch (long "pretend"
                    <> short 'p'
                    <> help "dry run: don't move or copy any files") <*>
               option auto (long "confidence"
                           <> short 'c'
                           <> value 1) <*>
               option auto (long "samples"
                           <> short 'N'
                           <> value 1) <*>
               strOption (long "torrent-hook"
                       <> value ""
                       <> help "Execute shell command for each processed torrent") <*>
               strOption (long "file-hook"
                       <> value ""
                       <> help "Execute shell command for each processed file") <*>
               strOption (long "output-dir"
                       <> short 'o'
                       <> value "./photobt"
                       <> help "Move output files there") <*>
               strOption (long "files"
                       <> short 'f'
                       <> value "."
                       <> metavar "files_directory") <*>
               many (strArgument $ metavar "torrent_file")

loadTorrents :: MyMonad [(FilePath, Torrent)]
loadTorrents =
  let
    readTorrent' path = do
      bin <- lift $ B.readFile path
      case readTorrent bin of
        Right b  -> return [(takeBaseName path, b)]
        Left err -> log 0 (printf "Couldn't load %s: %s" path (show err)) >>
                    return []
  in do
    paths <- asks _torrentFiles
    concat <$> mapM readTorrent' paths

torrentFiles' :: TorrentInfo -> [TorrentFile]
torrentFiles' SingleFile{tLength = l, tName = n} = [TorrentFile l [n]]
torrentFiles' MultiFile{tFiles = ff} = ff

torrentFiles :: TorrentInfo -> ToFiles
torrentFiles torrent =
  let
    toFile (offset, acc) tFile = ( offset + len
                                 , IM.insert int (int, path) acc
                                 )
        where path = bslToFilePath $ filePath tFile
              len = fileLength tFile
              int = offset `intInt` (offset + len)
  in
    snd $ foldl toFile (0, IM.empty) $ torrentFiles' torrent

type SzMatrix = Mx.Maptrix (FilePath, FilePath) FilePath Float

type SzIndex = M.Map Integer [FromFile]

mkSizeIndex :: [FromFile] -> SzIndex
mkSizeIndex = foldl (\m v -> M.insertWith (++) (_fromSize v) [v] m) M.empty

makeMatrix :: SzIndex
           -> [(FilePath, Torrent)]
           -> SzMatrix
makeMatrix fromFiles torrents =
  let
    szLookup a = case a `M.lookup` fromFiles of
                   Nothing -> []
                   Just x -> map _fromPath x

    sizeMatches = [ (((root, bslToFilePath toPath), fromPath), 1)
                  | (root, torrent) <- torrents
                  , TorrentFile sz toPath <- torrentFiles' $ tInfo torrent
                  , fromPath <- szLookup sz
                  ]
  in
    Mx.fromList sizeMatches

bslToFilePath = Txt.unpack . Txt.decodeUtf8 . B.toStrict . B.intercalate "/"

findFromFiles :: MyMonad [FromFile]
findFromFiles = do
  let go f = FromFile f <$> Dir.getFileSize f
  path <- asks _fileDir
  lift $ (toList . Dir.dirTree) <$> Dir.readDirectoryWith go path

readInterval :: (MonadIO m)
             => Interval Integer
             -> FilePath
             -> m B.ByteString
readInterval int path =
  liftIO $ withFile path ReadMode $ \h -> do
    let Finite offset = lowerBound int
    hSeek h AbsoluteSeek offset
    B.hGet h $ fromIntegral $ width int

sha1 :: B.ByteString -> B.ByteString
sha1 msg = B.fromChunks [BA.convert $ ((CR.hashlazy msg) :: CR.Digest CR.SHA1)]

assertEqual :: (Eq a, Show a)
            => String
            -> a
            -> a
            -> LogicT (ReaderT Config IO) ()
assertEqual msg a b = if a == b
                      then return ()
                      else do
                        lift $ log 5 (printf "Assert failed %s /= %s; %s" (show a) (show b) msg)
                        mzero

zeropad :: Integer -> [B.ByteString] -> B.ByteString
zeropad n b =
  let
    b' = B.concat b
    d  = (fromIntegral n) - (B.length b')
  in if d <= 0
     then b'
     else B.append b' (B.replicate d 0)

type Atom = (Interval Integer, FilePath)

filesInPiece :: ToFiles
             -> Interval Integer
             -> [Atom]
filesInPiece toFiles piece = [ (interval, path)
                             | (interval, (_, path)) <- IM.toList $ toFiles `IM.intersection` piece'
                             ]
  where piece' = IM.singleton piece (error "Should not happen (famous last words)")

-- Create list of file pieces to check:
pickBlocks :: Integer
           -> Integer
           -> ToFiles
           -> [(Integer, [Atom])]
pickBlocks numPieces pieceSize toFiles =
  let
    go i f | i == numPieces = []
           | otherwise      = c ++ go (i+1) f''
      where thisPiece = (offset `intInt` (offset + pieceSize))
            offset = pieceSize * i
            f' = filesInPiece toFiles thisPiece
            f'' = [b | (_, b) <- f']
            c = if f'' == f
                then []
                else [(i, f')]
  in nub $ go 0 []

blockMatchP :: B.ByteString
            -> [Atom]
            -> LogicT (ReaderT Config IO) ()
blockMatchP hash atoms = do
  piece <- lift $ B.concat <$> mapM (uncurry readInterval) atoms
  assertEqual (show atoms) hash (sha1 piece)

doBlock :: B.ByteString
        -> FilePath
        -> SzMatrix
        -> (Integer, [Atom])
        -> MyMonad SzMatrix
doBlock hashes torrentPath mtx (i, toAtoms) =
  let
    combine (_, a) (_, b) = (((torrentPath, a), b), 1)

    hash = B.take 20 $ B.drop (fromIntegral $ i*20) $ hashes

    candidates to = (M.keys $ mtx Mx.! (Mx.S (torrentPath, to), Mx.H)) :: [FilePath]
  in do
    results <- observeAllT $ do
      fromAtoms <- forM toAtoms $ \(interval, toPath) -> do
                                    fromPath <- msum $ map pure $ candidates toPath
                                    return (interval, fromPath)
      blockMatchP hash fromAtoms
      return $ zipWith combine toAtoms fromAtoms
    return $ foldl (Mx.updateWith (+)) mtx results

-- | Match real files with files in a torrent file
doTorrent :: SzMatrix
          -> (FilePath, Torrent)
          -> MyMonad SzMatrix
doTorrent mtx (torrentPath, Torrent{tInfo = torrent}) =
  let
    hashes    = tPieces torrent
    numPieces = (fromIntegral $ B.length hashes) `div` 20
    pieceSize = tPieceLength torrent
    pieces    = pickBlocks numPieces pieceSize $ torrentFiles torrent
  in do
    log 4 (printf "Checking %d/%d blocks in %s" (length pieces) numPieces torrentPath)
    log 6 (printf "Pieces: %s" $ show pieces)
    foldM (doBlock hashes torrentPath) mtx pieces

main :: IO ()
main = do
  config <- execParser $ info (helper <*> cliOptions) fullDesc
  (flip runReaderT) config $ do
    fromFiles <- findFromFiles
    log 7 (printf "fromFiles: %s" $ show fromFiles)
    torrents <- loadTorrents
    let sizeIndex  = mkSizeIndex fromFiles
        sizeMatrix = makeMatrix sizeIndex torrents
        initialMatches = length $ Mx.toList sizeMatrix
        ambiguous = [ r | r <- Mx.rows sizeMatrix
                        , (>1) $ M.size $ sizeMatrix Mx.! (Mx.S r, Mx.H)]
    log 3 (printf "Total number of files: %d" $ length fromFiles)
    log 3 (printf "Matching sizes: %d" initialMatches)
    log 3 (printf "Ambiguous cases: %d" $ length ambiguous)
    log 4 (printf "Ambiguous: %s" $ show ambiguous)
    log 7 (printf "preliminaryMatches: %s" $ show $ Mx.toList sizeMatrix)
    sizeMatrix' <- foldM doTorrent sizeMatrix torrents
    let matches = [(k2 :-> (root ++  '/':k1)) | (((root, k1), k2), v) <- Mx.toList sizeMatrix'
                                              , v >= _confidenceThreshold config
                                              ]
    log 2 (printf "Triaged: %d" $ length matches)
    lift $ completeWork config matches

completeWork :: Config -> [Match] -> IO ()
completeWork cfg matches
  | _dryRun cfg = forM_ matches $ \(a :-> b) -> (printf "%s -> %s\n" a b)
  | otherwise   =
  let
    action = Dir.copyFile
  in forM_ matches $ \(from :-> to) -> do
       let dest = _outDir cfg </> to
       Dir.createDirectoryIfMissing True $ takeDirectory dest
       printf "Copying %s to %s...\n" from dest
       action from dest

log :: Int -> String -> MyMonad ()
log severity str = do
  verbosity <- asks _verbosity
  if verbosity >= severity
  then lift $ Prelude.putStrLn str
  else return ()
