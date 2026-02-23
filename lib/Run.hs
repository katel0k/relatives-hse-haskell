module Run where

import qualified Data.HashMap.Strict as HashMap
import Rules (RulesMap, evaluateRule)
import Types (Vertex)

getRelatives :: RulesMap -> Vertex -> [(Vertex, String)]
getRelatives rulesMap entry =
  [ (v, role)
    | (role, rule) <- HashMap.toList rulesMap,
      v <- evaluateRule rule entry
  ]
