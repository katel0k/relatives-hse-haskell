module RulesList
  ( rules,
  )
where

import CommonRules (commonRules)
import qualified Data.HashMap.Strict as HashMap
import RuleUtils (technicalRules, a, d, f, i, m, o, s)
import Rules (RulesMap, (|:), mergeRules, (<==))

deeperRules :: RulesMap
deeperRules =
  HashMap.fromList
    [ "дядя" |: i a <== i a <== o m,
      "тётя" |: i a <== i a <== o f,
      "двоюродный брат/сестра" |: "родитель" <== "сиблинг" <== "ребенок",
      "прадедушка" |: "прародитель" <== i m,
      "прабабушка" |: "прародитель" <== i f,
      "правнук" |: "ребенок" <== "ребенок" <== o m,
      "правнучка" |: "ребенок" <== "ребенок" <== o f,
      "прадядя" |: "прародитель" <== i a <== o m,
      "пратётя" |: "прародитель" <== i a <== o f,
      "племянник" |: "сиблинг" <== o m,
      "племянница" |: "сиблинг" <== o f,
      "бывшая жена мужа (надеюсь)" |: d m <== o a <== i f,
      "бывший муж жены (надеюсь)" |: d f <== o a <== i m,
      "жена брата супруга" |: s <== "сиблинг" <== d f,
      "муж сестры супруга" |: s <== "сиблинг" <== d m,
      "внучатый племянник" |: "сиблинг" <== "ребенок" <== "ребенок" <== o m,
      "внучатая племянница" |: "сиблинг" <== "ребенок" <== "ребенок" <== o f,
      "двоюродный на поколение старше" |: "прародитель" <== "сиблинг" <== "ребенок",
      "двоюродный на поколение младше" |: "ребенок" <== "сиблинг" <== "ребенок"
    ]


rules :: RulesMap
rules = commonRules `mergeRules` deeperRules `mergeRules` technicalRules
