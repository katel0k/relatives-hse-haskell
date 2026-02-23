module Run where

import qualified Data.Map.Strict as Map
import Rules (Rule, evaluateRule)
import Types (Graph, Vertex, name)

getRelatives ::
  [(Rule, String)] ->
  Graph ->
  String ->
  Either String (Map.Map Vertex [String])
getRelatives ruleList graph entryName =
  case filter ((== entryName) . name) graph of
    [] -> Left ("Entry not found: " ++ entryName)
    entry ->
      let e = head entry
          pairs =
            [ (v, role)
              | (rule, role) <- ruleList,
                v <- evaluateRule rule e
            ]
          vertexToRoles = Map.fromListWith (++) (map (\(v, r) -> (v, [r])) pairs)
       in Right vertexToRoles
