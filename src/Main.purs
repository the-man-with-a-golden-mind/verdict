module Main where

import Prelude

import Data.Array (index)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log, error)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Node.Process (argv, setExitCode)
import Verdict.Compiler (compile)

-- | Node CLI entry point: `verdict <file.verdict>` → FinVM JSON on stdout.
-- | The actual compilation lives in the platform-agnostic `Verdict.Compiler`.
main :: Effect Unit
main = do
  args <- argv
  case index args 2 of
    Just filePath -> do
      src <- readTextFile UTF8 filePath
      case compile src of
        Left err -> do
          error err
          setExitCode 1
        Right jsonStr -> log jsonStr
    Nothing -> do
      error "Usage: spago run -- <file.verdict>"
      setExitCode 1
