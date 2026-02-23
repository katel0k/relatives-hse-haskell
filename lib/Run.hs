module Run where

import Rules (ResolvedRulesMap, evaluateRule)
import Types (Vertex)

-- | Evaluate all rules from a resolved list for a given vertex.
getRelatives :: ResolvedRulesMap -> Vertex -> [(Vertex, String)]
getRelatives resolved entry =
  [ (v, role)
    | (rule, role) <- resolved,
      v <- evaluateRule rule entry
  ]
