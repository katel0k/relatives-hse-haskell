module RulesList
  ( rules,
  )
where

import CommonRules (commonRules)
import qualified Data.HashMap.Strict as HashMap
import RuleUtils (a, d, f, i, m, o, p, c, s)
import Rules (RulesMap, mergeRules, (<==))

technicalRules :: RulesMap
technicalRules =
  HashMap.fromList
    [ ("прародитель", i a <== i a)
    ]

deeperRules :: RulesMap
deeperRules =
  HashMap.fromList
    [ ("дядя", i a <== i a <== o m),
      ("тётя", i a <== i a <== o f),
      ("двоюродный брат/сестра", i a <== i a <== o a <== o a),
      ("прадедушка", "прародитель" <== i m),
      ("прабабушка", "прародитель" <== i f),
      ("правнук", c <== c <== o m),
      ("правнучка", c <== c <== o f),
      ("прадядя", i a <== i a <== i a <== o m),
      ("пратётя", i a <== i a <== i a <== o f),
      ("племянник", i a <== o a <== o m),
      ("племянница", i a <== o a <== o f),
      ("бывшая жена мужа (надеюсь)", d m <== o a <== i f),
      ("бывший муж жены (надеюсь)", d f <== o a <== i m),
      ("жена брата супруга", s <== i a <== o m <== d f),
      ("муж сестры супруга", s <== i a <== o f <== d m),
      ("внучатый племянник", i a <== o a <== o a <== o m),
      ("внучатая племянница", i a <== o a <== o a <== o f),
      ("троюродный брат/сестра", i a <== i a <== i a <== o a <== o a <== o a),
      ("двоюродный на поколение старше", i a <== i a <== i a <== o a <== o a),
      ("двоюродный на поколение младше", i a <== i a <== o a <== o a <== o a)
    ]


rules :: RulesMap
rules = mergeRules (mergeRules commonRules deeperRules) technicalRules
