{-# LANGUAGE FlexibleInstances #-}

module Types where

import Data.Hashable (Hashable (..))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HS
import Control.Monad.State (State)

data Gender = Male | Female | Any deriving (Eq, Show)

data Vertex
  = Vertex
  { incoming :: [Vertex],
    outgoing :: [Vertex],
    dual :: [Vertex],
    name :: String,
    gender :: Gender
  }

type Graph = [Vertex]

instance Eq Vertex where
  (==) lhs rhs = name lhs == name rhs

instance Hashable Vertex where
  hashWithSalt salt v = hashWithSalt salt (name v)

instance Show Vertex where
  show (Vertex incoming_ outgoing_ dual_ name_ gender_) =
    unwords
      [ name_,
        show gender_,
        show $ map name incoming_,
        show $ map name outgoing_,
        show $ map name dual_
      ]

newtype ErrorMsg = ErrorMsg String deriving (Eq)

errorMessage :: ErrorMsg -> String
errorMessage (ErrorMsg s) = s

instance Show ErrorMsg where
  show (ErrorMsg s) = s

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
