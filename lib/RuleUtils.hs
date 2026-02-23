{-# OPTIONS_GHC -Wno-missing-signatures #-}

module RuleUtils
  ( m,
    f,
    a,
    i,
    o,
    d,
    p,
    c,
    s,
    technicalRules,
    technicalRuleNames,
  )
where

import qualified Data.HashMap.Strict as HashMap
import Rules (RulesMap, getAny, getFemales, getIncoming, getMales, getOutgoing, getSpouse, (|:), (<==))

-- | Get male vertices.
m = getMales

-- | Get female vertices.
f = getFemales

-- | Get any vertices.
a = getAny

-- | Get incoming vertices.
i = getIncoming

-- | Get outgoing vertices.
o = getOutgoing

-- | Get dual (spouse) vertices.
d = getSpouse

-- | Get any parent vertices.
p = i a

-- | Get any child vertices.
c = o a

-- | Get any spouse vertices.
s = d a

-- | Technical rules used only for building other rules (e.g. "прародитель").
-- Not exposed to the user; use technicalRuleNames to filter them out after resolution.
technicalRules :: RulesMap
technicalRules =
  HashMap.fromList
    [ "прародитель" |: i a <== i a
    ]

-- | Names of technical rules; exclude these when presenting resolved rules to the user.
technicalRuleNames :: [String]
technicalRuleNames = ["прародитель"]
