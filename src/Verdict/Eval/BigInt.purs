-- | Minimal arbitrary-precision integer arithmetic over decimal strings, used by
-- | the constant-folding pass. Backed by the host `BigInt`, which exists on both
-- | Node and browsers, so the compiler stays portable.
module Verdict.Eval.BigInt
  ( addStr
  , subStr
  , mulStr
  , divFloorStr
  , modStr
  , gcdStr
  , powStr
  , sqrtFloorStr
  , modPowStr
  , modInvStr
  , cmpStr
  , normalizeStr
  , scale10
  ) where

import Prelude

-- | a + b
foreign import addStr :: String -> String -> String

-- | a - b
foreign import subStr :: String -> String -> String

-- | a * b
foreign import mulStr :: String -> String -> String

-- | a / b, rounding toward negative infinity (the VM's "RoundDown").
foreign import divFloorStr :: String -> String -> String

-- | a modulo b, paired with floored division: a == divFloor(a,b) * b + mod(a,b).
foreign import modStr :: String -> String -> String

-- | gcd(|a|, |b|), always non-negative.
foreign import gcdStr :: String -> String -> String

-- | a^b for b >= 0.
foreign import powStr :: String -> String -> String

-- | floor(sqrt(a)) for a >= 0.
foreign import sqrtFloorStr :: String -> String

-- | modular exponentiation: base^exp mod m
foreign import modPowStr :: String -> String -> String -> String

-- | modular inverse of a modulo m, or 0 if no inverse exists.
foreign import modInvStr :: String -> String -> String

-- | compare a b -> -1 | 0 | 1
foreign import cmpStr :: String -> String -> Int

-- | Canonical decimal form (strips leading zeros / `+`), so equal values share
-- | one constant-pool slot.
foreign import normalizeStr :: String -> String

scale10 :: String -> Int -> String
scale10 v k = if k <= 0 then v else mulStr v ("1" <> pad k)
  where
  pad n = if n <= 0 then "" else "0" <> pad (n - 1)
