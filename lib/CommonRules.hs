module CommonRules
  ( commonRules,
  )
where

import Control.Monad ((>=>))
import RuleUtils (a, d, f, i, m, o)
import Rules (Rule (..))

commonRules :: [(Rule, String)]
commonRules =
  [ (Rule (i m), "отец"),
    (Rule (i f), "мать"),
    (Rule (o m), "сын"),
    (Rule (o f), "дочь"),
    (Rule (d m), "муж"),
    (Rule (d f), "жена"),
    (Rule (i a >=> i m), "дедушка"),
    (Rule (i a >=> i f), "бабушка"),
    (Rule (o a >=> o m), "внук"),
    (Rule (o a >=> o f), "внучка"),
    (Rule (i a >=> o m), "брат"),
    (Rule (i a >=> o f), "сестра"),
    (Rule (d a >=> i m), "свёкор"),
    (Rule (d a >=> i f), "свекровь")
  ]
