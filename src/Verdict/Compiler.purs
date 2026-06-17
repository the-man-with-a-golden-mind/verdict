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
  , runProjectWithLogsJS
  , signaturesJS
  , diagnosticsJS
  , runWithInputsJS
  , runProjectWithInputsJS
  , evalBindingsJS
  , programInputsJS
  ) where

import Prelude

import Data.Argonaut.Core (Json, stringifyWithIndent)
import Data.Argonaut.Encode (encodeJson)
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
import Verdict.FinVM.Types
  ( InputSchemaEntry(..)
  , ProgramVM
  , encodeProgramVM
  , inputSchemaDefaults
  , inputSchemaEntries
  )
import Verdict.Parser (parseModuleFull, parseVerdict)
import Verdict.Std.Link (link, linkAll)
import Verdict.Std.Prelude (preludeSource)
import Verdict.Std.Resolve (resolveProject)
import Verdict.Syntax.AST (Decl(..), Module, Name, ParsedModule, moduleDecls)
import Verdict.Typecheck (checkModule, locate, showTypeError)
import Verdict.VM.Eval (decodeInputValues, encodeValueJson, runProgramWithInputs, runProgramWithLogsWithInputs)

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
            Right (assemble emitFuncs lowered.entry lowered.inputs)

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
            Right (assemble emitFuncs lowered.entry lowered.inputs)

compileJson :: String -> Either String Json
compileJson src = map encodeProgramVM (compileProgram src)

runToJson :: String -> Either String Json
runToJson src = runToJsonWithInputs src FO.empty

runToJsonWithInputs :: String -> FO.Object Json -> Either String Json
runToJsonWithInputs src valuesJson = do
  values <- decodeInputValues valuesJson
  prog <- compileProgram src
  v <- runProgramWithInputs prog values
  pure (encodeValueJson v)

runProjectToJson :: FO.Object String -> Name -> Either String Json
runProjectToJson sources entry = runProjectToJsonWithInputs sources entry FO.empty

runProjectToJsonWithInputs :: FO.Object String -> Name -> FO.Object Json -> Either String Json
runProjectToJsonWithInputs sources entry valuesJson = do
  values <- decodeInputValues valuesJson
  prog <- compileProject sources entry
  v <- runProgramWithInputs prog values
  pure (encodeValueJson v)

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

runJS :: String -> FO.Object Json -> { ok :: Boolean, output :: String, error :: String }
runJS = runWithInputsJS

runProjectJS :: FO.Object String -> String -> FO.Object Json -> { ok :: Boolean, output :: String, error :: String }
runProjectJS = runProjectWithInputsJS

runWithInputsJS :: String -> FO.Object Json -> { ok :: Boolean, output :: String, error :: String }
runWithInputsJS src valuesJson =
  case compileProgram src of
    Left err -> { ok: false, output: "", error: err }
    Right prog -> case decodeInputValues valuesJson of
      Left err -> { ok: false, output: "", error: err }
      Right values -> case runProgramWithInputs prog values of
        Right v -> { ok: true, output: stringifyWithIndent 2 (encodeValueJson v), error: "" }
        Left err -> { ok: false, output: "", error: err }

runProjectWithInputsJS :: FO.Object String -> String -> FO.Object Json -> { ok :: Boolean, output :: String, error :: String }
runProjectWithInputsJS sources entry valuesJson =
  case compileProject sources entry of
    Left err -> { ok: false, output: "", error: err }
    Right prog -> case decodeInputValues valuesJson of
      Left err -> { ok: false, output: "", error: err }
      Right values -> case runProgramWithInputs prog values of
        Right v -> { ok: true, output: stringifyWithIndent 2 (encodeValueJson v), error: "" }
        Left err -> { ok: false, output: "", error: err }

--------------------------------------------------------------------------------
-- Editor integration: the entry points the playground/editor calls on the
-- bundled compiler (hover signatures, error squiggles, notebook inline results,
-- and the Run button with captured `sys.log` output).
--------------------------------------------------------------------------------

-- | Run a program on the reference VM and capture its `sys.log` output.
-- | Pass `{}` when no runtime overrides are needed (defaults still apply).
runWithLogsJS :: String -> FO.Object Json -> { ok :: Boolean, value :: String, error :: String, logs :: Array String }
runWithLogsJS src valuesJson = case compileProgram src of
  Left err -> { ok: false, value: "", error: err, logs: [] }
  Right prog ->
    case decodeInputValues valuesJson of
      Left err -> { ok: false, value: "", error: err, logs: [] }
      Right values ->
        let r = runProgramWithLogsWithInputs prog values
        in case r.result of
          Right v -> { ok: true, value: show v, error: "", logs: r.logs }
          Left err -> { ok: false, value: "", error: err, logs: r.logs }

runProjectWithLogsJS :: FO.Object String -> String -> FO.Object Json -> { ok :: Boolean, value :: String, error :: String, logs :: Array String }
runProjectWithLogsJS sources entry valuesJson =
  case compileProject sources entry of
    Left err -> { ok: false, value: "", error: err, logs: [] }
    Right prog ->
      case decodeInputValues valuesJson of
        Left err -> { ok: false, value: "", error: err, logs: [] }
        Right values ->
          let r = runProgramWithLogsWithInputs prog values
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
-- | Pass `{}` to rely on compiled input defaults only.
evalBindingsJS :: String -> FO.Object Json -> Array { name :: String, ok :: Boolean, value :: String, error :: String }
evalBindingsJS src valuesJson =
  case decodeInputValues valuesJson of
    Left err -> [ { name: "<inputs>", ok: false, value: "", error: err } ]
    Right values ->
      case parseVerdict src, compileBindings src of
        Right userMod, Right prog -> Array.mapMaybe (evalBinding values prog) (nullary userMod)
        _, _ -> []
  where
  nullary mod = Array.mapMaybe
    (\(Decl d) -> if Array.null d.params then Just d.name else Nothing)
    (moduleDecls mod)
  evalBinding values prog name =
    if FO.member name prog.functions then Just
      case runProgramWithInputs (prog { entrypoint = name }) values of
        Right v -> { name, ok: true, value: show v, error: "" }
        Left err -> { name, ok: false, value: "", error: err }
    else Nothing

type InputSchemaJS =
  { name :: String
  , type :: String
  , required :: Boolean
  }

-- | Introspect declared program inputs for editors and hosts (schema + defaults).
programInputsJS :: String -> { ok :: Boolean, schema :: Array InputSchemaJS, defaults :: FO.Object Json, error :: String }
programInputsJS src = case compileProgram src of
  Left err -> { ok: false, schema: [], defaults: FO.empty, error: err }
  Right prog -> case prog.inputs of
    Nothing -> { ok: true, schema: [], defaults: FO.empty, error: "" }
    Just ins ->
      { ok: true
      , schema: map schemaEntry (inputSchemaEntries ins)
      , defaults: map encodeJson (inputSchemaDefaults ins)
      , error: ""
      }
  where
  schemaEntry (InputSchemaEntry e) =
    { name: e.name
    , type: e.typeName
    , required: e.required
    }
