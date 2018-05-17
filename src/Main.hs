{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Main where

import Prelude hiding (readFile, log)
import qualified Data.ByteString.Lazy as B
import Text.Printf

import Data.Monoid
import Control.Applicative
import Options.Applicative
import Control.Monad.Reader

import Data.Torrent
import Data.Foldable (toList)
import Crypto.Hash
import Data.String
import qualified System.Directory as Dir
import qualified System.Directory.Tree as Dir
import qualified Data.Text.Encoding as Txt
import qualified Data.Text as Txt
import qualified Data.Map as M

type Hook = String

type MyMonad a = ReaderT Config IO a

data Config = Config
              { _copy           :: Bool
              , _verbosity      :: Int
              , _dryRun         :: Bool
              , _torrentHook
              , _fileHook       :: Hook
              , _outDir
              , _fileDir        :: FilePath
              , _torrentFiles   :: [FilePath]
              } deriving (Show)

data FromFile = FromFile
                { _fromPath :: FilePath
                , _fromSize :: Integer
                } deriving (Show)

data ToFile = ToFile
              { _torrentFile   :: FilePath
              , _toPath        :: [B.ByteString]
              , _toSize
              , _offset
              , _chunkSize     :: Integer
              } deriving (Show)

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

torrentFiles :: FilePath -> Torrent -> [ToFile]
torrentFiles fileName Torrent{tInfo = torrent} =
  let
    pieceLen = tPieceLength torrent
    dirName = case torrent of
                SingleFile{} ->
                  []
                MultiFile{tName = n} ->
                  [n]

    toFile (offset, acc) tFile = (offset + len, f : acc)
        where f = ToFile { _torrentFile = fileName
                         , _toPath      = dirName ++ filePath tFile
                         , _toSize      = len
                         , _offset      = offset
                         , _chunkSize   = pieceLen
                         }
              len = fileLength tFile
  in
  snd $ case torrent of
          SingleFile{tLength = l, tName = n} ->
            toFile (0, []) $ TorrentFile l [n]
          MultiFile{tFiles = ff} ->
            foldl toFile (0, []) ff

mkChunkMap :: [(FilePath, Torrent)] -> M.Map FilePath B.ByteString
mkChunkMap l = M.fromList [(k, tPieces $ tInfo v) | (k, v) <- l]

mkSizeIndex :: [ToFile] -> M.Map Integer [ToFile]
mkSizeIndex = foldl (\m v -> M.insertWith (++) (_toSize v) [v] m) M.empty

verify :: M.Map FilePath B.ByteString -> FromFile
       -> ToFile -> MyMonad [Match]
verify chunks f ToFile{ _torrentFile = tf, _toPath = path
                      , _offset = offset, _chunkSize = csz, _toSize = sz
                      } =
  let
    torrent     = chunks M.! tf
    offset'     = offset `rem` csz
    trustworthy = sz - offset' > csz
    fromPath    = _fromPath f
    toPath      = bslToFilePath path
  in if trustworthy
     then return [fromPath :-> toPath]
     else log 5 (printf "Quickcheck %s -> %s failed due to size: chunk=%d \
\offset'=%d size=%d" fromPath toPath csz offset' sz) >>
          return []

bslToFilePath = Txt.unpack . Txt.decodeUtf8 . B.toStrict . B.intercalate "/"

findFromFiles :: MyMonad [FromFile]
findFromFiles = do
  let go f = FromFile f <$> Dir.getFileSize f
  path <- asks _fileDir
  lift $ (toList . Dir.dirTree) <$> Dir.readDirectoryWith go path

main :: IO ()
main = do
  config <- execParser $ info (helper <*> cliOptions) fullDesc
  (flip runReaderT) config $ do
    torrents <- loadTorrents
    let toFiles    = torrents >>= uncurry torrentFiles
        chunks     = mkChunkMap torrents
        sizeIndex  = mkSizeIndex toFiles
    fromFiles <- findFromFiles
    log 7 (printf "fromFiles: %s" $ show fromFiles)
    log 7 (printf "sizeIndex: %s" $ show sizeIndex)
    let candidates1 = [(f, t) | f@FromFile{_fromSize = s} <- fromFiles
                              , t <- maybeToList $ s `M.lookup` sizeIndex
                              ]
    matches <- concat <$> mapM (uncurry (verify chunks)) candidates1
    log 1 (printf "Found %d matches" $ length matches)
    forM_ matches $ \(a :-> b) -> log 2 (printf "%s -> %s" a b)

maybeToList (Just a) = a
maybeToList Nothing  = []

log :: Int -> String -> MyMonad ()
log severity str = do
  verbosity <- asks _verbosity
  if verbosity >= severity
  then lift $ Prelude.putStrLn str
  else return ()
