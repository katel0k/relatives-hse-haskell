module Rules
  ( Rule (..),
    RulesMap,
    evaluateRule,
    mergeRules,
    getMales,
    getFemales,
    getAny,
    getIns,
    getOut,
    getDual,
  )
where

import Control.Monad ((>=>))
import Control.Monad.State (State, evalState, get, modify)
import Data.Foldable (Foldable (foldl'))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import Types

type Visited = HS.HashSet Vertex

newtype Rule = Rule ([Vertex] -> State Visited [Vertex])

type RulesMap = HashMap.HashMap String Rule

evaluateRule :: Rule -> Vertex -> [Vertex]
evaluateRule (Rule f) v = evalState (f [v]) (HS.singleton v)

mergeRules :: RulesMap -> RulesMap -> RulesMap
mergeRules a b = a `HashMap.union` b

unique :: (Hashable a) => [a] -> [a]
unique = HS.toList . HS.fromList

filterByPredicate :: (Vertex -> Bool) -> [Vertex] -> State Visited [Vertex]
filterByPredicate vertexPred vertices = do
  visited <- get
  modify (\s -> foldl' (flip HS.insert) s vertices)
  return $ unique $ filter (\v -> not (HS.member v visited) && vertexPred v) vertices

getMales :: [Vertex] -> State Visited [Vertex]
getMales = filterByPredicate (\v -> gender v == Male)

getFemales :: [Vertex] -> State Visited [Vertex]
getFemales = filterByPredicate (\v -> gender v == Female)

getAny :: [Vertex] -> State Visited [Vertex]
getAny = filterByPredicate (const True)

getIns :: ([Vertex] -> State Visited [Vertex]) -> [Vertex] -> State Visited [Vertex]
getIns genderGetter vertices = genderGetter (unique $ concatMap incoming vertices)

getOut :: ([Vertex] -> State Visited [Vertex]) -> [Vertex] -> State Visited [Vertex]
getOut genderGetter vertices = genderGetter (unique $ concatMap outgoing vertices)

getDual :: ([Vertex] -> State Visited [Vertex]) -> [Vertex] -> State Visited [Vertex]
getDual genderGetter vertices = genderGetter (unique $ concatMap dual vertices)
