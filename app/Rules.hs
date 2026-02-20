{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Rules where

import Types

import Control.Monad.State (State, get, modify)
import qualified Data.HashSet as HS
import Control.Monad ((>=>))

type Visited = HS.HashSet String

getMales :: [Vertex] -> State Visited [Vertex]
getMales vertices = do
  visited <- get
  modify (\s -> foldl (flip HS.insert) s (map name vertices))
  return $ filter (\v -> notElem (name v) visited && (gender v == Male)) vertices

getFemales :: [Vertex] -> State Visited [Vertex]
getFemales vertices = do
  visited <- get
  modify (\s -> foldl (flip HS.insert) s (map name vertices))
  return $ filter (\v -> notElem (name v) visited && (gender v == Female)) vertices

getAny :: [Vertex] -> State Visited [Vertex]
getAny = return

getIns :: ([Vertex] -> State Visited [Vertex]) -> [Vertex] -> State Visited [Vertex]
getIns genderGetter vertices = genderGetter (concatMap inc vertices)

getOut :: ([Vertex] -> State Visited [Vertex]) -> [Vertex] -> State Visited [Vertex]
getOut genderGetter vertices = genderGetter (concatMap out vertices)

getDual :: ([Vertex] -> State Visited [Vertex]) -> [Vertex] -> State Visited [Vertex]
getDual genderGetter vertices = genderGetter (concatMap dual vertices)

m = getMales
f = getFemales
a = getAny
i = getIns
o = getOut
d = getDual

rules :: [([Vertex] -> State Visited [Vertex], String)]
rules =
  [ (o m, "son"),
    (o a >=> o m, "grandson"),
    (i f, "mom"),
    (d f, "wife"),
    (i a >=> i a >=> o m, "uncle")
  ]
