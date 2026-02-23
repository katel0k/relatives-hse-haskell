import Control.Monad (forM_)
import Control.Monad.State (evalState)
import qualified Data.HashSet as HS
import GHC.IO.Encoding (utf8)
import Options.Applicative
import ParseInput
import Rules
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
  let entry = filter (\v -> name v == entryName) graph
      outputFunc = if debug then show else name
  case entry of
    [] -> putStrLn ("Entry not found: " ++ entryName) >> exitFailure
    (e : _) ->
      forM_ rules $ \(rule, role) -> do
        let result = evalState (rule entry) (HS.singleton e)
        forM_ result $ \v ->
          putStrLn $ outputFunc v ++ " " ++ role
