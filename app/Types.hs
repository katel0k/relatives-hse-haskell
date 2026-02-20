module Types where
import Data.Hashable (Hashable(..))

data Gender = Male | Female | Any deriving (Eq, Show)

data Vertex
  = Vertex
  { inc :: [Vertex],
    out :: [Vertex],
    dual :: [Vertex],
    name :: String,
    gender :: Gender
  }

instance Eq Vertex where
  (==) lhs rhs = name lhs == name rhs

instance Hashable Vertex where
  hashWithSalt salt v = hashWithSalt salt (name v)

instance Show Vertex where
  show (Vertex inc_ out_ dual_ name_ gender_) =
    unwords
      [ name_,
        show gender_,
        show $ map name inc_,
        show $ map name out_,
        show $ map name dual_
      ]
