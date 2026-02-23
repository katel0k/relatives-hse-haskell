module CommonRules
  ( commonRules,
  )
where

import qualified Data.HashMap.Strict as HashMap
import RuleUtils (a, d, f, i, m, o, p, c, s)
import Rules (RulesMap, (<==), toRuleExpr)

commonRules :: RulesMap
commonRules =
  HashMap.fromList
    [ ("отец", toRuleExpr (i m)),
      ("мать", toRuleExpr (i f)),
      ("сын", toRuleExpr (o m)),
      ("дочь", toRuleExpr (o f)),
      ("муж", toRuleExpr (d m)),
      ("жена", toRuleExpr (d f)),
      ("дедушка", p <== i m),
      ("бабушка", p <== i f),
      ("внук", c <== o m),
      ("внучка", c <== o f),
      ("брат", p <== o m),
      ("сестра", p <== o f),
      ("свёкор", s <== i m),
      ("свекровь", s <== i f)
    ]
