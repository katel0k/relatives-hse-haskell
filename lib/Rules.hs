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

filterByPredicate :: (Vertex -> Bool) -> [Vertex] -> State Visited [Vertex]
filterByPredicate vertexPred vertices = do
  visited <- get
  modify (\s -> foldl (flip HS.insert) s vertices)
  return $ unique $ filter (\v -> notElem v visited && vertexPred v) vertices

getMales :: [Vertex] -> State Visited [Vertex]
getMales = filterByPredicate (\v -> gender v == Male)

getFemales :: [Vertex] -> State Visited [Vertex]
getFemales = filterByPredicate (\v -> gender v == Female)

getAny :: [Vertex] -> State Visited [Vertex]
getAny = filterByPredicate (const True)

getIns :: ([Vertex] -> State Visited [Vertex]) -> [Vertex] -> State Visited [Vertex]
getIns genderGetter vertices = genderGetter (unique $ concatMap incoming vertices)

getOut :: ([Vertex] -> State Visited [Vertex]) -> [Vertex] -> State Visited [Vertex]
getOut genderGetter vertices = genderGetter (unique $ concatMap outgoing vertices)

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
  [ (i m, "отец"),
    (i f, "мать"),
    (o m, "сын"),
    (o f, "дочь"),
    (d m, "муж"),
    (d f, "жена"),
    (i a >=> i m, "дедушка"),
    (i a >=> i f, "бабушка"),
    (o a >=> o m, "внук"),
    (o a >=> o f, "внучка"),
    (i a >=> o m, "брат"),
    (i a >=> o f, "сестра"),
    (d a >=> i m, "свёкор"),
    (d a >=> i f, "свекровь"),
    (i a >=> i a >=> o m, "дядя"),
    (i a >=> i a >=> o f, "тётя"),
    (i a >=> i a >=> o a >=> o a, "двоюродный брат/сестра"),
    (i a >=> i a >=> i m, "прадедушка"),
    (i a >=> i a >=> i f, "прабабушка"),
    (o a >=> o a >=> o m, "правнук"),
    (o a >=> o a >=> o f, "правнучка"),
    (i a >=> i a >=> i a >=> o m, "прадядя"),
    (i a >=> i a >=> i a >=> o f, "пратётя"),
    (i a >=> o a >=> o m, "племянник"),
    (i a >=> o a >=> o f, "племянница"),
    (d m >=> o a >=> i f, "бывшая жена мужа (надеюсь)"),
    (d f >=> o a >=> i m, "бывший муж жены (надеюсь)"),
    (d a >=> i a >=> o m >=> d f, "жена брата супруга"),
    (d a >=> i a >=> o f >=> d m, "муж сестры супруга"),
    (i a >=> o a >=> o a >=> o m, "внучатый племянник"),
    (i a >=> o a >=> o a >=> o f, "внучатая племянница"),
    (i a >=> i a >=> i a >=> o a >=> o a >=> o a, "троюродный брат/сестра"),
    (i a >=> i a >=> i a >=> o a >=> o a, "двоюродный на поколение старше"),
    (i a >=> i a >=> o a >=> o a >=> o a, "двоюродный на поколение младше")
  ]
