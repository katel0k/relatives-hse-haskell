module CommonRules
  ( commonRules,
  )
where

import RuleUtils (a, d, f, i, m, o, p, c, s)
import Rules (RulesMap, (|:), (<==), fromList)

commonRules :: RulesMap
commonRules =
  fromList
    [ "отец" |: i m,
      "мать" |: i f,
      "сын" |: o m,
      "дочь" |: o f,
      "муж" |: d m,
      "жена" |: d f,
      "дедушка" |: p <== i m,
      "бабушка" |: p <== i f,
      "внук" |: c <== o m,
      "внучка" |: c <== o f,
      "брат" |: p <== o m,
      "сестра" |: p <== o f,
      "свёкор" |: s <== i m,
      "свекровь" |: s <== i f
    ]
