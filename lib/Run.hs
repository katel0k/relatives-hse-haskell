module Run where

import Rules (Rule, evaluateRule)
import Types (Vertex)

getRelatives :: [(Rule, String)] -> Vertex -> [(Vertex, String)]
getRelatives ruleList entry =
  [ (v, role)
    | (rule, role) <- ruleList,
      v <- evaluateRule rule entry
  ]
