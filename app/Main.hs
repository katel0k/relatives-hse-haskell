import Control.Monad (forM_)
import Control.Monad.State (evalState)
import qualified Data.HashSet as HS
import Options.Applicative
import ParseInput
import Rules
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
  do
    content <- readFile filename
    let graph = parseInput content
        entry = filter (\v -> name v == entryName) graph
        outputFunc = if debug then show else name
    forM_ rules $ \(rule, role) -> do
      let result = evalState (rule entry) HS.empty
      forM_ result $ \v -> do
        putStrLn $ outputFunc v ++ " " ++ role
