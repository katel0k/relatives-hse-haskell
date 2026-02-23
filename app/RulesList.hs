module RulesList
  ( rules,
  )
where

import CommonRules (commonRules)
import Control.Monad ((>=>))
import RuleUtils (a, d, f, i, m, o)
import Rules (Rule (..), mergeRules)

deeperRules :: [(Rule, String)]
deeperRules =
  [ (Rule (i a >=> i a >=> o m), "дядя"),
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

rules :: [(Rule, String)]
rules = mergeRules commonRules deeperRules
