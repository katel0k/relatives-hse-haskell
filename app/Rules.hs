{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Rules where

import Control.Monad ((>=>))
import Control.Monad.State (State, get, modify)
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import Types

type Visited = HS.HashSet Vertex

unique :: (Hashable a) => [a] -> [a]
unique = HS.toList . HS.fromList

getMales :: [Vertex] -> State Visited [Vertex]
getMales vertices = do
  visited <- get
  modify (\s -> foldl (flip HS.insert) s vertices)
  return $ unique $ filter (\v -> notElem v visited && (gender v == Male)) vertices

getFemales :: [Vertex] -> State Visited [Vertex]
getFemales vertices = do
  visited <- get
  modify (\s -> foldl (flip HS.insert) s vertices)
  return $ unique $ filter (\v -> notElem v visited && (gender v == Female)) vertices

getAny :: [Vertex] -> State Visited [Vertex]
getAny vertices = do
  visited <- get
  modify (\s -> foldl (flip HS.insert) s vertices)
  return $ unique $ filter (`notElem` visited) vertices

getIns :: ([Vertex] -> State Visited [Vertex]) -> [Vertex] -> State Visited [Vertex]
getIns genderGetter vertices = genderGetter (unique $ concatMap inc vertices)

getOut :: ([Vertex] -> State Visited [Vertex]) -> [Vertex] -> State Visited [Vertex]
getOut genderGetter vertices = genderGetter (unique $ concatMap out vertices)

getDual :: ([Vertex] -> State Visited [Vertex]) -> [Vertex] -> State Visited [Vertex]
getDual genderGetter vertices = genderGetter (unique $ concatMap dual vertices)

m = getMales

f = getFemales

a = getAny

i = getIns

o = getOut

d = getDual

rules :: [([Vertex] -> State Visited [Vertex], String)]
rules =
  [ (i m, "father"),
    (i f, "mother"),
    (o m, "son"),
    (o f, "daughter"),
    (d m, "husband"),
    (d f, "wife"),
    (i a >=> i m, "grandfather"),
    (i a >=> i f, "grandmother"),
    (o a >=> o m, "grandson"),
    (o a >=> o f, "granddaughter"),
    (i a >=> o m, "brother"),
    (i a >=> o f, "sister"),
    (d a >=> i m, "father-in-law"),
    (d a >=> i f, "mother-in-law"),
    (i a >=> i a >=> o m, "uncle"),
    (i a >=> i a >=> o f, "aunt"),
    (i a >=> i a >=> o a >=> o a, "cousin"),
    (i a >=> i a >=> i m, "great-grandfather"),
    (i a >=> i a >=> i f, "great-grandmother"),
    (o a >=> o a >=> o m, "great-grandson"),
    (o a >=> o a >=> o f, "great-granddaughter"),
    (i a >=> i a >=> i a >=> o m, "great-uncle"),
    (i a >=> i a >=> i a >=> o f, "great-aunt"),
    (i a >=> o a >=> o m, "nephew"),
    (i a >=> o a >=> o f, "niece"),
    (d m >=> o a >=> i f, "husband's ex-wife (hopefully)"),
    (d f >=> o a >=> i m, "wife's ex-husband (hopefully)"),
    (d a >=> i a >=> o m >=> d f, "brother-in-law's wife"),
    (d a >=> i a >=> o f >=> d m, "sister-in-law's husband"),
    (i a >=> o a >=> o a >=> o m, "grandnephew"),
    (i a >=> o a >=> o a >=> o f, "grandniece"),
    (i a >=> i a >=> i a >=> o a >=> o a >=> o a, "second cousin"),
    (i a >=> i a >=> i a >=> o a >=> o a, "first cousin once removed (older)"),
    (i a >=> i a >=> o a >=> o a >=> o a, "first cousin once removed (younger)")
  ]
