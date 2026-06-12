-- | Small host-regex helpers for the deterministic reference VM. Invalid
-- | patterns return a conservative fallback instead of throwing through the VM.
module Verdict.Eval.Regex
  ( regexTest
  , regexFindAll
  , regexReplace
  , regexSplit
  ) where

foreign import regexTest :: String -> String -> Boolean

foreign import regexFindAll :: String -> String -> Array String

foreign import regexReplace :: String -> String -> String -> String

foreign import regexSplit :: String -> String -> Array String
