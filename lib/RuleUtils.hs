{-# OPTIONS_GHC -Wno-missing-signatures #-}

module RuleUtils
  ( m,
    f,
    a,
    i,
    o,
    d,
  )
where

import Rules (getAny, getDual, getFemales, getIns, getMales, getOut)

m = getMales

f = getFemales

a = getAny

i = getIns

o = getOut

d = getDual
