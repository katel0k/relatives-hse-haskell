module ParseInput where

import qualified Data.Map.Strict as Map
import Types

parseInput :: String -> Either ErrorMsg [Vertex]
parseInput input =
  let ls = lines input
      (sec1, rest1) = break null ls
      rest1' = dropWhile null rest1
      (sec2, rest2) = break null rest1'
      rest2' = dropWhile null rest2
      (sec3, _) = break null rest2'
   in do
        genderPairs <- _parseGender sec1
        parentChildPairs <- _parseParentChild sec2
        marriagePairs <- _parseMarriage sec3
        return $ _buildGraph genderPairs parentChildPairs marriagePairs

_parseGender :: [String] -> Either ErrorMsg [(String, Gender)]
_parseGender = traverse parseLine . zip ([1 ..] :: [Int])
  where
    parseLine (n, line) = case words line of
      [name_, "M"] -> Right (name_, Male)
      [name_, "F"] -> Right (name_, Female)
      _ -> Left (ErrorMsg ("Line " ++ show n ++ ": invalid gender line: " ++ line))

_parseParentChild :: [String] -> Either ErrorMsg [(String, String)]
_parseParentChild = traverse parseLine . zip ([1 ..] :: [Int])
  where
    parseLine (n, line) = case words line of
      [p, "->", c] -> Right (p, c)
      _ -> Left (ErrorMsg ("Line " ++ show n ++ ": invalid parent-child line: " ++ line))

_parseMarriage :: [String] -> Either ErrorMsg [(String, String)]
_parseMarriage = traverse parseLine . zip ([1 ..] :: [Int])
  where
    parseLine (n, line) = case words line of
      [a, "<->", b] -> Right (a, b)
      _ -> Left (ErrorMsg ("Line " ++ show n ++ ": invalid marriage line: " ++ line))

_buildGraph ::
  [(String, Gender)] -> -- names to genders mapping
  [(String, String)] -> -- parent ->  child edges
  [(String, String)] -> -- spouse <-> spouse edges
  [Vertex]
_buildGraph genderPairs parentChildPairs marriagePairs =
  let genderMap = Map.fromList genderPairs
      allNames = Map.keys genderMap

      (parentMap, childMap) = foldl addParentChild (Map.empty, Map.empty) parentChildPairs

      spouseMap = foldl addMarriage Map.empty marriagePairs

      -- works due to laziness
      vertexMap =
        Map.fromList
          [ ( name_,
              Vertex
                { incoming = map (vertexMap Map.!) (findOrGetEmpty name_ parentMap),
                  outgoing = map (vertexMap Map.!) (findOrGetEmpty name_ childMap),
                  dual = map (vertexMap Map.!) (findOrGetEmpty name_ spouseMap),
                  name = name_,
                  gender = genderMap Map.! name_
                }
            )
            | name_ <- allNames
          ]
   in Map.elems vertexMap
  where
    findOrGetEmpty = Map.findWithDefault []

    addParentChild (pMap, cMap) (p, c) =
      ( Map.insertWith (++) c [p] pMap,
        Map.insertWith (++) p [c] cMap
      )

    addMarriage sMap (a, b) =
      Map.insertWith (++) a [b] $ Map.insertWith (++) b [a] sMap
