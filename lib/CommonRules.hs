module CommonRules
  ( commonRules,
  )
where

import RuleUtils (a, d, f, i, m, o, p, c, s)
import Rules (RulesMap, (|:), (==>), fromList)

commonRules :: RulesMap
commonRules =
  fromList
    [ i m       |: "отец",
      i f       |: "мать",
      o m       |: "сын",
      o f       |: "дочь",
      d m       |: "муж",
      d f       |: "жена",
      i m ==> p |: "дедушка",
      i f ==> p |: "бабушка",
      o m ==> c |: "внук",
      o f ==> c |: "внучка",
      o m ==> p |: "брат",
      o f ==> p |: "сестра",
      i m ==> s |: "свекор",
      i f ==> s |: "свекровь"
    ]
