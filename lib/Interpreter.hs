module Interpreter where

import Control.Monad (forM_)
import GetInput (readFromFile)
import ParseInput (parseInput)
import RuleUtils (technicalRuleNames)
import Rules
  ( RuleExpr,
    RulesMap,
    ToRuleExpr,
    fromList,
    mergeRules,
    resolveRulesMap,
    withoutTechnicalRules,
    (|:),
  )
import Run (getRelatives)
import Types (Vertex, name)

findRelativesInFile :: String -> RulesMap -> String -> IO [(Vertex, String)]
findRelativesInFile filename rules entryName = do
  content <- readFromFile filename
  let Right graph = parseInput content
  let Right resolvedRules = resolveRulesMap rules
  let [entryVertex] = filter ((== entryName) . name) graph
  let resolvedForUser = withoutTechnicalRules technicalRuleNames resolvedRules
  return $ getRelatives resolvedForUser entryVertex

prettyPrint :: [(Vertex, String)] -> IO ()
prettyPrint pairs = do
  forM_ pairs (\(v, role) -> putStrLn (name v ++ " " ++ role))

addRule :: (ToRuleExpr a) => RulesMap -> String -> a -> RulesMap
addRule rules ruleName ruleExpr = rules `mergeRules` fromList [ruleName |: ruleExpr]
