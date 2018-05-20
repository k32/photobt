{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Main where

import Prelude hiding (readFile, log)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteArray as BA
import Text.Printf
import System.IO (withFile, IOMode(..), SeekMode(..), hSeek)

import Data.Monoid
import Control.Applicative
import Options.Applicative
import Control.Monad.Reader

import Data.Torrent
import Data.Foldable (toList)
import qualified Crypto.Hash as CR
import Data.String
import qualified System.Directory as Dir
import qualified System.Directory.Tree as Dir
import qualified Data.Text.Encoding as Txt
import qualified Data.Text as Txt
import qualified Data.Map as M
import Control.Monad.Logic
import Control.Monad.Plus
import Data.List (foldl1')

import Data.Interval
import qualified Data.IntervalMap.Lazy as IM

intInt :: Integer -> Integer -> Interval Integer
intInt a b = Finite a <=..< Finite b

type Hook = String

type MyMonad a = ReaderT Config IO a

data Config =
    Config
    { _copy           :: Bool
    , _verbosity      :: Int
    , _dryRun         :: Bool
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

type ToFiles = IM.IntervalMap Integer (Interval Integer, FilePath)

data Match = FilePath :-> FilePath
             deriving (Show)

cliOptions :: Parser Config
cliOptions = Config <$>
               switch (long "copy"
                    <> short 'C'
                    <> help "copy files instead of moving") <*>
               option auto (long "verbose"
                         <> short 'v') <*>
               switch (long "pretend"
                    <> short 'p'
                    <> help "dry run") <*>
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
        Right b  -> return [(path, b)]
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

mkSizeIndex :: [FromFile] -> M.Map Integer [FromFile]
mkSizeIndex = foldl (\m v -> M.insertWith (++) (_fromSize v) [v] m) M.empty

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
        

matchFiles :: M.Map Integer [FromFile]
           -> (FilePath, Torrent)
           -> MyMonad [Match]
matchFiles fromFiles (torrentPath, Torrent{tInfo = torrent}) =
  let
    -- progress i success = return ()
    --     -- | (i*80) `rem` numPieces == 0 = liftIO $ putChar (if success then '.' else 'x')
    --     -- | otherwise = return ()

    hashes    = tPieces torrent
    numPieces = (fromIntegral $ B.length hashes) `div` 20
    pieceSize = tPieceLength torrent
    toFiles   = torrentFiles torrent

    hash i = B.take 20 $ B.drop (fromIntegral $ i * 20) hashes

    sameSize int = mfromList =<< mfromMaybe (M.lookup (width int) fromFiles)

    tryFiles candidates hash = do
      fromFiles <- mapM (liftM _fromPath . sameSize . fst . snd) candidates
      let atoms' = zip (map fst candidates) fromFiles
      piece <- B.concat <$> mapM (uncurry readInterval) atoms'
      assertEqual (printf "files=%s" (show candidates)) hash (sha1 piece)
      let candidates' = [path | (int, (trueInt, path)) <- candidates
                              , upperBound trueInt == upperBound int
                              ]
      return $ zipWith (:->) fromFiles candidates'

    go i acc
        | i == numPieces = return acc
        | otherwise      =
            let
              offset = i * pieceSize
              pieceEnd = offset + pieceSize
              piece = IM.singleton (offset `intInt` pieceEnd)
                                   (error "Should not happen")
              files = IM.toList $ toFiles `IM.intersection` piece
            in ifte (tryFiles files $ hash i)
                    (\match -> go (i+1) $ match ++ acc)
                    (go (i+1) acc)
  in do
    log 3 (printf "%s: pieceSize=%d numPieces=%d"
                  torrentPath
                  pieceSize
                  numPieces)
    log 8 (printf "%s files: %s"
                  torrentPath
                  (show toFiles))
    observeT $ go 0 []
                             
main :: IO ()
main = do
  config <- execParser $ info (helper <*> cliOptions) fullDesc
  (flip runReaderT) config $ do
    fromFiles <- findFromFiles    
    log 7 (printf "fromFiles: %s" $ show fromFiles)
    torrents <- loadTorrents
    let sizeIndex  = mkSizeIndex fromFiles
    matches <- concat <$> mapM (matchFiles sizeIndex) torrents
    log 1 (printf "Found %d matches" $ length matches)
    forM_ matches $ \(a :-> b) -> log 2 (printf "%s -> %s" a b)

log :: Int -> String -> MyMonad ()
log severity str = do
  verbosity <- asks _verbosity
  if verbosity >= severity
  then lift $ Prelude.putStrLn str
  else return ()
