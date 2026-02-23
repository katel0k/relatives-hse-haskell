module Types where

import Data.Hashable (Hashable (..))

data Gender = Male | Female | Any deriving (Eq, Show)

data Vertex
  = Vertex
  { incoming :: [Vertex],
    outgoing :: [Vertex],
    dual :: [Vertex],
    name :: String,
    gender :: Gender
  }

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
