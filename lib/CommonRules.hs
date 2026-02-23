module CommonRules
  ( commonRules,
  )
where

import Control.Monad ((>=>))
import qualified Data.HashMap.Strict as HashMap
import RuleUtils (a, d, f, i, m, o)
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
      ("дедушка", Rule (i a >=> i m)),
      ("бабушка", Rule (i a >=> i f)),
      ("внук", Rule (o a >=> o m)),
      ("внучка", Rule (o a >=> o f)),
      ("брат", Rule (i a >=> o m)),
      ("сестра", Rule (i a >=> o f)),
      ("свёкор", Rule (d a >=> i m)),
      ("свекровь", Rule (d a >=> i f))
    ]
