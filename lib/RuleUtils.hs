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
    s
  )
where

import Rules (getAny, getDual, getFemales, getIns, getMales, getOut)

-- male
m = getMales

-- female
f = getFemales

-- any
a = getAny

-- incoming
i = getIns

-- outgoing
o = getOut

-- dual
d = getDual

-- parent
p = i a

-- child
c = o a

-- spouse
s = d a
