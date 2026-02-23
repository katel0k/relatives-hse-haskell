module RulesList
  ( rules,
  )
where

import CommonRules (commonRules)
import Control.Monad ((>=>))
import qualified Data.HashMap.Strict as HashMap
import RuleUtils (a, d, f, i, m, o)
import Rules (Rule (..), RulesMap, mergeRules)

deeperRules :: RulesMap
deeperRules =
  HashMap.fromList
    [ ("дядя", Rule (i a >=> i a >=> o m)),
      ("тётя", Rule (i a >=> i a >=> o f)),
      ("двоюродный брат/сестра", Rule (i a >=> i a >=> o a >=> o a)),
      ("прадедушка", Rule (i a >=> i a >=> i m)),
      ("прабабушка", Rule (i a >=> i a >=> i f)),
      ("правнук", Rule (o a >=> o a >=> o m)),
      ("правнучка", Rule (o a >=> o a >=> o f)),
      ("прадядя", Rule (i a >=> i a >=> i a >=> o m)),
      ("пратётя", Rule (i a >=> i a >=> i a >=> o f)),
      ("племянник", Rule (i a >=> o a >=> o m)),
      ("племянница", Rule (i a >=> o a >=> o f)),
      ("бывшая жена мужа (надеюсь)", Rule (d m >=> o a >=> i f)),
      ("бывший муж жены (надеюсь)", Rule (d f >=> o a >=> i m)),
      ("жена брата супруга", Rule (d a >=> i a >=> o m >=> d f)),
      ("муж сестры супруга", Rule (d a >=> i a >=> o f >=> d m)),
      ("внучатый племянник", Rule (i a >=> o a >=> o a >=> o m)),
      ("внучатая племянница", Rule (i a >=> o a >=> o a >=> o f)),
      ("троюродный брат/сестра", Rule (i a >=> i a >=> i a >=> o a >=> o a >=> o a)),
      ("двоюродный на поколение старше", Rule (i a >=> i a >=> i a >=> o a >=> o a)),
      ("двоюродный на поколение младше", Rule (i a >=> i a >=> o a >=> o a >=> o a))
    ]

rules :: RulesMap
rules = mergeRules commonRules deeperRules
