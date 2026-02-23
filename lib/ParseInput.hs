module ParseInput where

import Data.Char (isSpace)
import qualified Data.Map.Strict as Map
import Types

dropCommentLines :: [String] -> [String]
dropCommentLines = filter (not . isComment)
  where
    isComment line = case dropWhile isSpace line of
      ('#' : _) -> True
      _ -> False

parseInput :: String -> Either ErrorMsg [Vertex]
parseInput input =
  let ls = dropCommentLines (lines input)
      (sec1, rest1) = break null ls
      rest1' = dropWhile null rest1
      (sec2, rest2) = break null rest1'
      rest2' = dropWhile null rest2
      (sec3, _) = break null rest2'
   in do
        genderPairs <- _parseGender sec1
        marriagePairs <- _parseMarriage sec2
        parentChildPairs <- _parseParentChild sec3
        return $ _buildGraph genderPairs parentChildPairs marriagePairs

_parseGender :: [String] -> Either ErrorMsg [(String, Gender)]
_parseGender = traverse parseLine . zip ([1 ..] :: [Int])
  where
    parseLine (n, line) =
      let ws = words line
       in if length ws >= 2
            then case last ws of
              "(М)" -> Right (unwords (init ws), Male)
              "(Ж)" -> Right (unwords (init ws), Female)
              _ -> Left (ErrorMsg ("Line " ++ show n ++ ": invalid gender line (expected (М) or (Ж)): " ++ line))
            else Left (ErrorMsg ("Line " ++ show n ++ ": invalid gender line: " ++ line))

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

      vertexMap =
        Map.fromList
          [ (name_, vertexWithEdges name_ vertexMap)
            | name_ <- allNames
          ]

      vertexWithEdges name_ vm =
        let findEntryByName = Map.findWithDefault [] name_
            parentNames = findEntryByName parentMap
            childNames = findEntryByName childMap
            spouseNames = findEntryByName spouseMap
            getVertexByName = (vm Map.!)
         in Vertex
              { incoming = map getVertexByName parentNames,
                outgoing = map getVertexByName childNames,
                dual = map getVertexByName spouseNames,
                name = name_,
                gender = genderMap Map.! name_
              }
   in Map.elems vertexMap
  where
    addParentChild (pMap, cMap) (p, c) =
      ( Map.insertWith (++) c [p] pMap,
        Map.insertWith (++) p [c] cMap
      )

    addMarriage sMap (a, b) =
      Map.insertWith (++) a [b] $ Map.insertWith (++) b [a] sMap
