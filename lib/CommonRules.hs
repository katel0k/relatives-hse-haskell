module CommonRules
  ( commonRules,
  )
where

import Control.Monad ((>=>))
import qualified Data.HashMap.Strict as HashMap
import RuleUtils (a, d, f, i, m, o, p, c, s)
import Rules (Rule (..), RulesMap)

commonRules :: RulesMap
commonRules =
  HashMap.fromList
    [ ("отец", Rule (i m)),
      ("мать", Rule (i f)),
      ("сын", Rule (o m)),
      ("дочь", Rule (o f)),
      ("муж", Rule (d m)),
      ("жена", Rule (d f)),
      ("дедушка", Rule (p >=> i m)),
      ("бабушка", Rule (p >=> i f)),
      ("внук", Rule (c >=> o m)),
      ("внучка", Rule (c >=> o f)),
      ("брат", Rule (p >=> o m)),
      ("сестра", Rule (p >=> o f)),
      ("свёкор", Rule (s >=> i m)),
      ("свекровь", Rule (s >=> i f))
    ]
