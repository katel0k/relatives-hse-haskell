import qualified Data.Map.Strict as Map
import Control.Monad (forM_)

data Gender = Male | Female | Any deriving (Eq, Show)

data Vertex =
    Vertex {
        inc :: [Vertex],
        out :: [Vertex],
        dual :: [Vertex],
        name :: String,
        gender :: Gender
    }

instance Show Vertex where
    show (Vertex inc_ out_ dual_ name_ gender_) =
        unwords [
            name_, show gender_,
            show $ map name inc_,
            show $ map name out_,
            show $ map name dual_
        ]

getMales :: [Vertex] -> [Vertex]
getMales = filter (\v -> gender v == Male)
getFemales :: [Vertex] -> [Vertex]
getFemales = filter (\v -> gender v == Female)
getAny :: [Vertex] -> [Vertex]
getAny = id

getIns :: ([Vertex] -> [Vertex]) -> [Vertex] -> [Vertex]
getIns genderGetter vertices = genderGetter (concatMap inc vertices)
getOut :: ([Vertex] -> [Vertex]) -> [Vertex] -> [Vertex]
getOut genderGetter vertices = genderGetter (concatMap out vertices)
getDual :: ([Vertex] -> [Vertex]) -> [Vertex] -> [Vertex]
getDual genderGetter vertices = genderGetter (concatMap dual vertices)

m = getMales
f = getFemales
a = getAny

i = getIns
o = getOut
d = getDual

rules :: [([Vertex] -> [Vertex], String)]
rules = [
    (o m, "son"),
    (o a . o m, "grandson"),
    (i f, "mom"),
    (d f, "wife"),
    (i a . i a . o m, "uncle")
    ]

-- | Main parsing function.
parseInput :: String -> [Vertex]
parseInput input =
    let ls = lines input
        -- Split into three sections separated by blank lines.
        (sec1, rest1)   = break null ls
        rest1'          = dropWhile null rest1
        (sec2, rest2)   = break null rest1'
        rest2'          = dropWhile null rest2
        (sec3, _)       = break null rest2'

        genderPairs     = parseGender sec1
        parentChildPairs = parseParentChild sec2
        marriagePairs   = parseMarriage sec3
    in buildGraph genderPairs parentChildPairs marriagePairs

-- | Parse the first section: "name M" or "name F"
parseGender :: [String] -> [(String, Gender)]
parseGender = map parseLine
  where
    parseLine line = case words line of
        [name_, "M"] -> (name_, Male)
        [name_, "F"] -> (name_, Female)
        _           -> error ("Invalid gender line: " ++ line)

-- | Parse the second section: "parent -> child"
parseParentChild :: [String] -> [(String, String)]
parseParentChild = map parseLine
  where
    parseLine line = case words line of
        [p, "->", c] -> (p, c)
        _            -> error ("Invalid parent-child line: " ++ line)

-- | Parse the third section: "person1 <-> person2"
parseMarriage :: [String] -> [(String, String)]
parseMarriage = map parseLine
  where
    parseLine line = case words line of
        [a, "<->", b] -> (a, b)
        _             -> error ("Invalid marriage line: " ++ line)

-- | Build the cyclic graph from the parsed data.
buildGraph :: [(String, Gender)]      -- names and genders
           -> [(String, String)]      -- parent → child edges
           -> [(String, String)]      -- spouse ↔ spouse edges
           -> [Vertex]
buildGraph genderPairs parentChildPairs marriagePairs =
    let genderMap = Map.fromList genderPairs
        allNames  = Map.keys genderMap   -- every vertex must appear here

        -- Build parent map (child → [parents]) and child map (parent → [children])
        (parentMap, childMap) = foldl addParentChild (Map.empty, Map.empty) parentChildPairs

        -- Build spouse map (person → [spouses])
        spouseMap = foldl addMarriage Map.empty marriagePairs

        -- Knot‑tying: create a map from name to Vertex, where each Vertex's
        -- fields refer to other Vertices by looking them up in the same map.
        vertexMap = Map.fromList
            [ (name_, Vertex
                { inc   = map (vertexMap Map.!) (Map.findWithDefault [] name_ parentMap)
                , out   = map (vertexMap Map.!) (Map.findWithDefault [] name_ childMap)
                , dual  = map (vertexMap Map.!) (Map.findWithDefault [] name_ spouseMap)
                , name  = name_
                , gender = genderMap Map.! name_
                })
            | name_ <- allNames ]
    in Map.elems vertexMap

  where
    addParentChild (pMap, cMap) (p, c) =
        ( Map.insertWith (++) c [p] pMap
        , Map.insertWith (++) p [c] cMap
        )

    addMarriage sMap (a, b) =
        Map.insertWith (++) a [b] $ Map.insertWith (++) b [a] sMap

main :: IO ()
main = do
    n <- readFile "test.txt"
    let graph = parseInput n
    let entry = filter (\v -> name v == "A") graph
    forM_ rules $ \(rule, role) -> do
        forM_ (rule entry) $ \v -> do
            putStrLn $ show v ++ " " ++ role
