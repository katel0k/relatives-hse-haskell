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
  )
where

import Rules (getAny, getDual, getFemales, getIns, getMales, getOut)

-- | Get male vertices.
m = getMales

-- | Get female vertices.
f = getFemales

-- | Get any vertices.
a = getAny

-- | Get incoming vertices.
i = getIns

-- | Get outgoing vertices.
o = getOut

-- | Get dual vertices.
d = getDual

-- | Get any parent vertices.
p = i a

-- | Get any child vertices.
c = o a

-- | Get any spouse vertices.
s = d a
