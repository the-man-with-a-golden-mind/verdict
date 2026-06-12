module Verdict.Compiler
  ( compile
  , compileJson
  , compileProgram
  , compileProject
  , compileJS
  , compileProjectJS
  , runToJson
  , runProjectToJson
  , runJS
  , runProjectJS
  ) where

import Prelude

import Data.Argonaut.Core (Json, stringifyWithIndent)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Foreign.Object as FO
import Verdict.Core.Lower (lowerModule)
import Verdict.Core.MIR (MFunc)
import Verdict.Core.Monomorph (monomorphize)
import Verdict.Core.Opt (inlineNullaries, optimize)
import Verdict.Core.Regalloc (allocate)
import Verdict.FinVM.Emit (EmitFunc, assemble)
import Verdict.FinVM.Types (ProgramVM, encodeProgramVM)
import Verdict.Parser (parseModuleFull, parseVerdict)
import Verdict.Std.Link (link)
import Verdict.Std.Prelude (preludeSource)
import Verdict.Std.Resolve (resolveProject)
import Verdict.Syntax.AST (Module, Name, ParsedModule)
import Verdict.Typecheck (checkModule, showTypeError)
import Verdict.VM.Eval (encodeValueJson, runProgram)

-- | The full pipeline, pure and IO-free, so it runs unchanged on Node and in
-- | the browser:
-- |
-- |   parse → typecheck → lower to MIR → optimize → register-allocate → assemble

-- | Compile a single-file program to the in-memory FinVM program.
compileProgram :: String -> Either String ProgramVM
compileProgram src = case parseVerdict src of
  Left err -> Left ("Parse error: " <> show err)
  Right userMod -> finish userMod

-- | Compile a multi-file project: a map of module-name → source plus the entry
-- | module name. Resolution merges everything reachable through `import`s into a
-- | single module, then the normal pipeline runs. The compiler stays IO-free —
-- | the CLI reads files and builds the map.
compileProject :: FO.Object String -> Name -> Either String ProgramVM
compileProject sources entry = do
  parsed <- parseAll sources
  merged <- resolveProject parsed entry
  finish merged

parseAll :: FO.Object String -> Either String (Map Name ParsedModule)
parseAll sources =
  map Map.fromFoldable (traverse parseOne entries)
  where
  entries = FO.toUnfoldable sources :: Array (Tuple Name String)

  parseOne :: Tuple Name String -> Either String (Tuple Name ParsedModule)
  parseOne (Tuple name src) = case parseModuleFull src of
    Left err -> Left ("Parse error in '" <> name <> "': " <> show err)
    Right pm -> Right (Tuple name pm)

-- | The shared back half: link the prelude, typecheck, monomorphize, lower,
-- | optimize, and assemble the merged module.
finish :: Module -> Either String ProgramVM
finish userMod =
  case parseVerdict preludeSource of
    Left err -> Left ("Internal error: prelude failed to parse: " <> show err)
    Right preludeMod ->
      let mod = link userMod preludeMod
      in case checkModule mod of
        Left tyErr -> Left ("Type error: " <> showTypeError tyErr)
        Right _ ->
          let
            mono = monomorphize mod
            lowered = lowerModule mono
            inlined = inlineNullaries lowered.funcs lowered.entry
            emitFuncs = map (toEmitFunc <<< optimize) inlined
          in
            Right (assemble emitFuncs lowered.entry)

compileJson :: String -> Either String Json
compileJson src = map encodeProgramVM (compileProgram src)

runToJson :: String -> Either String Json
runToJson src = map encodeValueJson (compileProgram src >>= runProgram)

runProjectToJson :: FO.Object String -> Name -> Either String Json
runProjectToJson sources entry =
  map encodeValueJson (compileProject sources entry >>= runProgram)

-- | optimize → allocate → package the per-function record the assembler wants.
toEmitFunc :: MFunc -> EmitFunc
toEmitFunc f =
  let alloc = allocate f
  in
    { name: f.name
    , arity: Array.length f.params
    , paramTys: f.paramTys
    , retTy: f.retTy
    , registerCount: alloc.registerCount
    , body: alloc.body
    , isEntry: f.isEntry
    }

compile :: String -> Either String String
compile src = map (stringifyWithIndent 2) (compileJson src)

compileJS :: String -> { ok :: Boolean, output :: String, error :: String }
compileJS src = case compile src of
  Right out -> { ok: true, output: out, error: "" }
  Left err -> { ok: false, output: "", error: err }

-- | JS-friendly multi-file entry: the CLI passes a plain object of
-- | module-name → source and the entry module name.
compileProjectJS :: FO.Object String -> String -> { ok :: Boolean, output :: String, error :: String }
compileProjectJS sources entry =
  case map (stringifyWithIndent 2 <<< encodeProgramVM) (compileProject sources entry) of
    Right out -> { ok: true, output: out, error: "" }
    Left err -> { ok: false, output: "", error: err }

runJS :: String -> { ok :: Boolean, output :: String, error :: String }
runJS src = case runToJson src of
  Right json -> { ok: true, output: stringifyWithIndent 2 json, error: "" }
  Left err -> { ok: false, output: "", error: err }

runProjectJS :: FO.Object String -> String -> { ok :: Boolean, output :: String, error :: String }
runProjectJS sources entry = case runProjectToJson sources entry of
  Right json -> { ok: true, output: stringifyWithIndent 2 json, error: "" }
  Left err -> { ok: false, output: "", error: err }
