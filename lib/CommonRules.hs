module CommonRules
  ( commonRules,
  )
where

import RuleUtils (a, d, f, i, m, o, p, c, s)
import Rules (RulesMap, (|:), (==>), fromList)

commonRules :: RulesMap
commonRules =
  fromList
    [ "отец" |: i m,
      "мать" |: i f,
      "сын" |: o m,
      "дочь" |: o f,
      "муж" |: d m,
      "жена" |: d f,
      "дедушка" |: i m ==> p,
      "бабушка" |: i f ==> p,
      "внук" |: o m ==> c,
      "внучка" |: o f ==> c,
      "брат" |: o m ==> p,
      "сестра" |: o f ==> p,
      "свёкор" |: i m ==> s,
      "свекровь" |: i f ==> s
    ]
