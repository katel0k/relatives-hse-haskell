import Control.Monad (forM_)
import Options.Applicative
import ParseInput
import Rules (resolveRulesMap, withoutTechnicalRules)
import RulesList (rules)
import RuleUtils (technicalRuleNames)
import Run
import System.Exit (exitFailure)
import Types (errorMessage, name)
import GetInput (readFromFile, readUtf8Strict)
import System.IO (stdin)

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
      Just filename -> readFromFile filename
  graph <-
    case parseInput content of
      Left err -> putStrLn (errorMessage err) >> exitFailure
      Right g -> return g
  resolvedRules <-
    case resolveRulesMap rules of
      Left err -> putStrLn (errorMessage err) >> exitFailure
      Right r -> return r
  entryVertex <-
    case filter ((== entryName) . name) graph of
      [] -> putStrLn ("Entry not found: " ++ entryName) >> exitFailure
      (v : _) -> return v
  let resolvedForUser = withoutTechnicalRules technicalRuleNames resolvedRules
      pairs = getRelatives resolvedForUser entryVertex
      outputLine v role = putStrLn (name v ++ " " ++ role)
      outputLineDebug v role = print (name v, role)
  if debug
    then forM_ pairs $ uncurry outputLineDebug
    else forM_ pairs $ uncurry outputLine
