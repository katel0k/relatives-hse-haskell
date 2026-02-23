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

import Rules (getAny, getFemales, getIncoming, getMales, getOutgoing, getSpouse)

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
