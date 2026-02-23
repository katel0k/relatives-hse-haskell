import Control.Monad (forM_)
import qualified Data.Map.Strict as Map
import GHC.IO.Encoding (utf8)
import Options.Applicative
import ParseInput
import Run
import Rules (rules)
import System.Exit (exitFailure)
import System.IO (Handle, IOMode (ReadMode), hGetContents, hSetEncoding, stdin, withFile)
import Types

data Options = Options
  { optEntry :: String,
    optFilename :: Maybe FilePath,
    optDebug :: Bool
  }

options :: Parser Options
options =
  Options
    <$> argument str (metavar "ENTRY" <> help "Entry name")
    <*> optional (argument str (metavar "FILE" <> help "Input graph file (omit to read from stdin)"))
    <*> switch (long "debug" <> help "Enable debug output")

readUtf8Strict :: Handle -> IO String
readUtf8Strict h = do
  hSetEncoding h utf8
  c <- hGetContents h
  last (c ++ "\0") `seq` return c

main :: IO ()
main = do
  opts <-
    execParser $
      info
        (options <**> helper)
        (fullDesc <> progDesc "Process graph with rules" <> header "mygraph")
  let entryName = optEntry opts
      debug = optDebug opts
  content <-
    case optFilename opts of
      Nothing -> readUtf8Strict stdin
      Just filename -> withFile filename ReadMode readUtf8Strict
  graph <-
    case parseInput content of
      Left err -> putStrLn (errorMessage err) >> exitFailure
      Right g -> return g
  vertexToRoles <-
    case getRelatives rules graph entryName of
      Left err -> putStrLn err >> exitFailure
      Right m -> return m
  let outputLine v roles = forM_ roles $ \role -> putStrLn (name v ++ " " ++ role)
      outputLineDebug v roles = forM_ roles $ \role -> print (name v, role)
  if debug
    then forM_ (Map.toList vertexToRoles) $ uncurry outputLineDebug
    else forM_ (Map.toList vertexToRoles) $ uncurry outputLine
