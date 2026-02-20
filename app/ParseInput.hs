module ParseInput where

import Types
import qualified Data.Map.Strict as Map

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
buildGraph :: [(String, Gender)]      -- names to genders mapping
           -> [(String, String)]      -- parent ->  child edges
           -> [(String, String)]      -- spouse <-> spouse edges
           -> [Vertex]
buildGraph genderPairs parentChildPairs marriagePairs =
    let genderMap = Map.fromList genderPairs
        allNames  = Map.keys genderMap

        (parentMap, childMap) = foldl addParentChild (Map.empty, Map.empty) parentChildPairs

        spouseMap = foldl addMarriage Map.empty marriagePairs

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
