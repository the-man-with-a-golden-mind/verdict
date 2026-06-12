module Verdict.Eval.Rational
  ( Rat
  , add
  , cmp
  , divR
  , mul
  , reduce
  , render
  , sub
  ) where

import Prelude

import Verdict.Eval.BigInt (addStr, cmpStr, divFloorStr, gcdStr, mulStr, normalizeStr, subStr)

type Rat = { num :: String, den :: String }

reduce :: String -> String -> Rat
reduce n0 d0 =
  let
    n = normalizeStr n0
    d = normalizeStr d0
  in
    if cmpStr n "0" == 0 then { num: "0", den: "1" }
    else
      let
        signed =
          if cmpStr d "0" < 0 then { num: negateStr n, den: negateStr d }
          else { num: n, den: d }
        g = gcdStr signed.num signed.den
      in
        if cmpStr g "0" == 0 then signed
        else
          { num: divFloorStr signed.num g
          , den: divFloorStr signed.den g
          }

add :: Rat -> Rat -> Rat
add a b =
  reduce
    (addStr (mulStr a.num b.den) (mulStr b.num a.den))
    (mulStr a.den b.den)

sub :: Rat -> Rat -> Rat
sub a b =
  reduce
    (subStr (mulStr a.num b.den) (mulStr b.num a.den))
    (mulStr a.den b.den)

mul :: Rat -> Rat -> Rat
mul a b =
  reduce (mulStr a.num b.num) (mulStr a.den b.den)

divR :: Rat -> Rat -> Rat
divR a b =
  reduce (mulStr a.num b.den) (mulStr a.den b.num)

cmp :: Rat -> Rat -> Int
cmp a b =
  let
    ar = reduce a.num a.den
    br = reduce b.num b.den
  in
    cmpStr (mulStr ar.num br.den) (mulStr br.num ar.den)

render :: Rat -> String
render r =
  let rr = reduce r.num r.den
  in rr.num <> "/" <> rr.den

negateStr :: String -> String
negateStr = subStr "0"
