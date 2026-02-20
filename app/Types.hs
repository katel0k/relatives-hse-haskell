module Types where

data Gender = Male | Female | Any deriving (Eq, Show)

data Vertex
  = Vertex
  { inc :: [Vertex],
    out :: [Vertex],
    dual :: [Vertex],
    name :: String,
    gender :: Gender
  }

instance Show Vertex where
  show (Vertex inc_ out_ dual_ name_ gender_) =
    unwords
      [ name_,
        show gender_,
        show $ map name inc_,
        show $ map name out_,
        show $ map name dual_
      ]
