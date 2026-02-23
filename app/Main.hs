import Control.Monad (forM_)
import Control.Monad.State (evalState)
import qualified Data.HashSet as HS
import GHC.IO.Encoding (utf8)
import Options.Applicative
import ParseInput
import Rules
import System.Exit (exitFailure)
import System.IO (IOMode (ReadMode), hGetContents, hSetEncoding, withFile)
import Types

data Options = Options
  { optFilename :: FilePath,
    optEntry :: String,
    optDebug :: Bool
  }

options :: Parser Options
options =
  Options
    <$> argument str (metavar "FILENAME" <> help "Input graph file")
    <*> argument str (metavar "ENTRY" <> help "Entry name")
    <*> switch (long "debug" <> help "Enable debug output")

main :: IO ()
main = do
  opts <-
    execParser $
      info
        (options <**> helper)
        (fullDesc <> progDesc "Process graph with rules" <> header "mygraph")
  let filename = optFilename opts
      entryName = optEntry opts
      debug = optDebug opts
  content <- withFile filename ReadMode $ \h -> do
    hSetEncoding h utf8
    hGetContents h
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
