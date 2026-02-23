{-# LANGUAGE FlexibleInstances #-}

module Rules
  ( Rule,
    RuleExpr,
    ResolvedRulesMap,
    RulesMap,
    evaluateRule,
    mergeRules,
    makeRule,
    resolveRulesMap,
    withoutTechnicalRules,
    getMales,
    getFemales,
    getAny,
    getIncoming,
    getOutgoing,
    getSpouse,
    (<==),
    ToRuleExpr (..),
    RulePart (..),
    (|:),
  )
where

import Control.Monad ((>=>))
import Control.Monad.State (State, evalState, get, modify)
import Data.Foldable (Foldable (foldl'))
import Data.Traversable (traverse)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import Types

type Visited = HS.HashSet Vertex

type Rule = [Vertex] -> State Visited [Vertex]

type RulePart = Either String Rule

type RuleExpr = [RulePart]

type RulesMap = HashMap.HashMap String RuleExpr

type ResolvedRulesMap = [(Rule, String)]

class ToRuleExpr a where
  toRuleExpr :: a -> RuleExpr

instance ToRuleExpr RulePart where
  toRuleExpr p = [p]

instance ToRuleExpr RuleExpr where
  toRuleExpr = id

instance ToRuleExpr Rule where
  toRuleExpr r = [Right r]

instance ToRuleExpr String where
  toRuleExpr s = [Left s]

infixr 5 <==
(<==) :: (ToRuleExpr a, ToRuleExpr b) => a -> b -> RuleExpr
a <== b = toRuleExpr a ++ toRuleExpr b

infix 2 |:
(|:) :: ToRuleExpr a => String -> a -> (String, RuleExpr)
(|:) n e = (n, toRuleExpr e)

makeRule :: RulesMap -> RuleExpr -> Either ErrorMsg Rule
makeRule rules parts = do
  resolved <- traverse (resolvePart rules) parts
  return $ foldr (>=>) return resolved
  where
    resolvePart _ (Right r) = Right r
    resolvePart rulesMap (Left name) =
      case HashMap.lookup name rulesMap of
        Nothing -> Left $ ErrorMsg $ "Rule " ++ name ++ " not found"
        Just expr -> makeRule rulesMap expr

evaluateRule :: Rule -> Vertex -> [Vertex]
evaluateRule f v = evalState (f [v]) (HS.singleton v)

mergeRules :: RulesMap -> RulesMap -> RulesMap
mergeRules a b = a `HashMap.union` b

-- | Resolve all rule expressions in the map to concrete rules.
resolveRulesMap :: RulesMap -> Either ErrorMsg ResolvedRulesMap
resolveRulesMap rulesMap =
  traverse
    ( \(name, expr) ->
        case makeRule rulesMap expr of
          Left err -> Left $ ErrorMsg $ "Error while resolving rule " ++ name ++ ": " ++ errorMessage err
          Right r -> Right (r, name)
    )
    (HashMap.toList rulesMap)

withoutTechnicalRules :: [String] -> ResolvedRulesMap -> ResolvedRulesMap
withoutTechnicalRules technicalNames = filter (\(_, name) -> name `notElem` technicalNames)

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

getIncoming :: ([Vertex] -> State Visited [Vertex]) -> [Vertex] -> State Visited [Vertex]
getIncoming genderGetter vertices = genderGetter (unique $ concatMap incoming vertices)

getOutgoing :: ([Vertex] -> State Visited [Vertex]) -> [Vertex] -> State Visited [Vertex]
getOutgoing genderGetter vertices = genderGetter (unique $ concatMap outgoing vertices)

getSpouse :: ([Vertex] -> State Visited [Vertex]) -> [Vertex] -> State Visited [Vertex]
getSpouse genderGetter vertices = genderGetter (unique $ concatMap dual vertices)
