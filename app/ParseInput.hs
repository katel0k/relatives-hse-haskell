module ParseInput where

import qualified Data.Map.Strict as Map
import Types

parseInput :: String -> [Vertex]
parseInput input =
  let ls = lines input
      -- Split into three sections separated by blank lines.
      (sec1, rest1) = break null ls
      rest1' = dropWhile null rest1
      (sec2, rest2) = break null rest1'
      rest2' = dropWhile null rest2
      (sec3, _) = break null rest2'

      genderPairs = _parseGender sec1
      parentChildPairs = _parseParentChild sec2
      marriagePairs = _parseMarriage sec3
   in buildGraph genderPairs parentChildPairs marriagePairs

_parseGender :: [String] -> [(String, Gender)]
_parseGender = map parseLine
  where
    parseLine line = case words line of
      [name_, "M"] -> (name_, Male)
      [name_, "F"] -> (name_, Female)
      _ -> error ("Invalid gender line: " ++ line)

_parseParentChild :: [String] -> [(String, String)]
_parseParentChild = map parseLine
  where
    parseLine line = case words line of
      [p, "->", c] -> (p, c)
      _ -> error ("Invalid parent-child line: " ++ line)

_parseMarriage :: [String] -> [(String, String)]
_parseMarriage = map parseLine
  where
    parseLine line = case words line of
      [a, "<->", b] -> (a, b)
      _ -> error ("Invalid marriage line: " ++ line)

buildGraph ::
  [(String, Gender)] -> -- names to genders mapping
  [(String, String)] -> -- parent ->  child edges
  [(String, String)] -> -- spouse <-> spouse edges
  [Vertex]
buildGraph genderPairs parentChildPairs marriagePairs =
  let genderMap = Map.fromList genderPairs
      allNames = Map.keys genderMap

      (parentMap, childMap) = foldl addParentChild (Map.empty, Map.empty) parentChildPairs

      spouseMap = foldl addMarriage Map.empty marriagePairs

      vertexMap =
        Map.fromList
          [ ( name_,
              Vertex
                { inc = map (vertexMap Map.!) (Map.findWithDefault [] name_ parentMap),
                  out = map (vertexMap Map.!) (Map.findWithDefault [] name_ childMap),
                  dual = map (vertexMap Map.!) (Map.findWithDefault [] name_ spouseMap),
                  name = name_,
                  gender = genderMap Map.! name_
                }
            )
            | name_ <- allNames
          ]
   in Map.elems vertexMap
  where
    addParentChild (pMap, cMap) (p, c) =
      ( Map.insertWith (++) c [p] pMap,
        Map.insertWith (++) p [c] cMap
      )

    addMarriage sMap (a, b) =
      Map.insertWith (++) a [b] $ Map.insertWith (++) b [a] sMap
