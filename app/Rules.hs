module Rules where

import Types

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

m :: [Vertex] -> [Vertex]
m = getMales

f :: [Vertex] -> [Vertex]
f = getFemales

a :: [Vertex] -> [Vertex]
a = getAny

i :: ([Vertex] -> [Vertex]) -> [Vertex] -> [Vertex]
i = getIns

o :: ([Vertex] -> [Vertex]) -> [Vertex] -> [Vertex]
o = getOut

d :: ([Vertex] -> [Vertex]) -> [Vertex] -> [Vertex]
d = getDual

rules :: [([Vertex] -> [Vertex], String)]
rules =
  [ (o m, "son"),
    (o a . o m, "grandson"),
    (i f, "mom"),
    (d f, "wife"),
    (i a . i a . o m, "uncle")
  ]
