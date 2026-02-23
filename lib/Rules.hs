{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Rules
  ( Rule,
    evaluateRule,
    rules,
  )
where

import Control.Monad ((>=>))
import Control.Monad.State (State, evalState, get, modify)
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import Types

type Visited = HS.HashSet Vertex

newtype Rule = Rule ([Vertex] -> State Visited [Vertex])

evaluateRule :: Rule -> Vertex -> [Vertex]
evaluateRule (Rule f) v = evalState (f [v]) (HS.singleton v)

unique :: (Hashable a) => [a] -> [a]
unique = HS.toList . HS.fromList

filterByPredicate :: (Vertex -> Bool) -> [Vertex] -> State Visited [Vertex]
filterByPredicate vertexPred vertices = do
  visited <- get
  modify (\s -> foldl (flip HS.insert) s vertices)
  return $ unique $ filter (\v -> not (HS.member v visited) && vertexPred v) vertices

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

rules :: [(Rule, String)]
rules =
  [ (Rule (i m), "отец"),
    (Rule (i f), "мать"),
    (Rule (o m), "сын"),
    (Rule (o f), "дочь"),
    (Rule (d m), "муж"),
    (Rule (d f), "жена"),
    (Rule (i a >=> i m), "дедушка"),
    (Rule (i a >=> i f), "бабушка"),
    (Rule (o a >=> o m), "внук"),
    (Rule (o a >=> o f), "внучка"),
    (Rule (i a >=> o m), "брат"),
    (Rule (i a >=> o f), "сестра"),
    (Rule (d a >=> i m), "свёкор"),
    (Rule (d a >=> i f), "свекровь"),
    (Rule (i a >=> i a >=> o m), "дядя"),
    (Rule (i a >=> i a >=> o f), "тётя"),
    (Rule (i a >=> i a >=> o a >=> o a), "двоюродный брат/сестра"),
    (Rule (i a >=> i a >=> i m), "прадедушка"),
    (Rule (i a >=> i a >=> i f), "прабабушка"),
    (Rule (o a >=> o a >=> o m), "правнук"),
    (Rule (o a >=> o a >=> o f), "правнучка"),
    (Rule (i a >=> i a >=> i a >=> o m), "прадядя"),
    (Rule (i a >=> i a >=> i a >=> o f), "пратётя"),
    (Rule (i a >=> o a >=> o m), "племянник"),
    (Rule (i a >=> o a >=> o f), "племянница"),
    (Rule (d m >=> o a >=> i f), "бывшая жена мужа (надеюсь)"),
    (Rule (d f >=> o a >=> i m), "бывший муж жены (надеюсь)"),
    (Rule (d a >=> i a >=> o m >=> d f), "жена брата супруга"),
    (Rule (d a >=> i a >=> o f >=> d m), "муж сестры супруга"),
    (Rule (i a >=> o a >=> o a >=> o m), "внучатый племянник"),
    (Rule (i a >=> o a >=> o a >=> o f), "внучатая племянница"),
    (Rule (i a >=> i a >=> i a >=> o a >=> o a >=> o a), "троюродный брат/сестра"),
    (Rule (i a >=> i a >=> i a >=> o a >=> o a), "двоюродный на поколение старше"),
    (Rule (i a >=> i a >=> o a >=> o a >=> o a), "двоюродный на поколение младше")
  ]
