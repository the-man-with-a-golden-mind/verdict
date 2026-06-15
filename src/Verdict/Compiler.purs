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
  , runWithLogsJS
  , signaturesJS
  , diagnosticsJS
  , evalBindingsJS
  ) where

import Prelude

import Data.Argonaut.Core (Json, stringifyWithIndent)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Foreign.Object as FO
import Parsing (Position(..), parseErrorMessage, parseErrorPosition)
import Verdict.Core.Lower (lowerModule)
import Verdict.Core.MIR (MFunc)
import Verdict.Core.Monomorph (monomorphize)
import Verdict.Core.Opt (inlineNullaries, optimize)
import Verdict.Core.Regalloc (allocate)
import Verdict.FinVM.Emit (EmitFunc, assemble)
import Verdict.FinVM.Types (ProgramVM, encodeProgramVM)
import Verdict.Parser (parseModuleFull, parseVerdict)
import Verdict.Std.Link (link, linkAll)
import Verdict.Std.Prelude (preludeSource)
import Verdict.Std.Resolve (resolveProject)
import Verdict.Syntax.AST (Decl(..), Module, Name, ParsedModule, moduleDecls)
import Verdict.Typecheck (checkModule, locate, showTypeError)
import Verdict.VM.Eval (encodeValueJson, runProgram, runProgramWithLogs)

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

-- | A build for editor introspection: keeps EVERY top-level binding (so each can
-- | be evaluated independently) and skips nullary inlining (so a value binding
-- | survives as its own runnable function).
compileBindings :: String -> Either String ProgramVM
compileBindings src = case parseVerdict src of
  Left err -> Left ("Parse error: " <> show err)
  Right userMod -> case parseVerdict preludeSource of
    Left _ -> Left "internal error: prelude failed to parse"
    Right preludeMod ->
      let mod = linkAll userMod preludeMod
      in case checkModule mod of
        Left tyErr -> Left ("Type error: " <> showTypeError tyErr)
        Right _ ->
          let
            lowered = lowerModule (monomorphize mod)
            emitFuncs = map (toEmitFunc <<< optimize) lowered.funcs
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

--------------------------------------------------------------------------------
-- Editor integration: the entry points the playground/editor calls on the
-- bundled compiler (hover signatures, error squiggles, notebook inline results,
-- and the Run button with captured `sys.log` output).
--------------------------------------------------------------------------------

-- | Run a program on the reference VM and capture its `sys.log` output.
runWithLogsJS :: String -> { ok :: Boolean, value :: String, error :: String, logs :: Array String }
runWithLogsJS src = case compileProgram src of
  Left err -> { ok: false, value: "", error: err, logs: [] }
  Right prog ->
    let r = runProgramWithLogs prog
    in case r.result of
      Right v -> { ok: true, value: show v, error: "", logs: r.logs }
      Left err -> { ok: false, value: "", error: err, logs: r.logs }

-- | The type signature of each top-level binding (user source + prelude), so the
-- | editor can show it on hover.
signaturesJS :: String -> Array { name :: String, signature :: String }
signaturesJS src = sigsOf src <> sigsOf preludeSource
  where
  sigsOf code = case parseVerdict code of
    Left _ -> []
    Right mod -> Array.mapMaybe declSig (moduleDecls mod)
  declSig (Decl d) = map (\t -> { name: d.name, signature: show t }) d.sig

-- | Parse / type errors with positions, for editor squiggles. Reports the first
-- | error (positions are relative to the user source).
diagnosticsJS :: String -> Array { line :: Int, column :: Int, message :: String, severity :: String }
diagnosticsJS src = case parseVerdict src of
  Left perr -> [ parseDiag perr ]
  Right userMod -> case parseVerdict preludeSource of
    Left _ -> []
    Right preludeMod -> case checkModule (link userMod preludeMod) of
      Left tyErr ->
        let l = locate tyErr
        in [ { line: l.line, column: l.column, message: l.message, severity: "error" } ]
      Right _ -> []
  where
  parseDiag perr = case parseErrorPosition perr of
    Position p -> { line: p.line, column: p.column, message: parseErrorMessage perr, severity: "error" }

-- | Evaluate every nullary top-level binding on the reference VM (editor notebook
-- | inline results). Bindings unreachable from the entry are skipped.
evalBindingsJS :: String -> Array { name :: String, ok :: Boolean, value :: String, error :: String }
evalBindingsJS src = case parseVerdict src, compileBindings src of
  Right userMod, Right prog -> Array.mapMaybe (evalBinding prog) (nullary userMod)
  _, _ -> []
  where
  nullary mod = Array.mapMaybe
    (\(Decl d) -> if Array.null d.params then Just d.name else Nothing)
    (moduleDecls mod)
  evalBinding prog name =
    if FO.member name prog.functions then Just
      case runProgram (prog { entrypoint = name }) of
        Right v -> { name, ok: true, value: show v, error: "" }
        Left err -> { name, ok: false, value: "", error: err }
    else Nothing
