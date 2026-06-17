-- | Assembly: allocated MIR (physical registers) -> FinVM ProgramVM. Builds the
-- | program-wide, deduplicated constant pool and translates each MIR instruction
-- | to its bytecode form. All optimization already happened upstream.
module Verdict.FinVM.Emit (EmitFunc, assemble) where

import Prelude

import Control.Monad.State (State, get, put, runState)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (takeWhile) as SCU
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Foreign.Object as FO
import Verdict.Core.MIR (MInstr(..))
import Verdict.Eval.Rational as Rational
import Verdict.FinVM.Types (FunctionVM, InputSchemaEntry(..), InputsVM(..), InstructionVM(..), ProgramVM, TypeVM(..), ValueVM(..))
import Data.Maybe (Maybe(..), isNothing)
import Verdict.Syntax.AST (BinOp(..), CmpOp(..), Expr(..), InputDecl(..), Lit(..), Ty(..))

type EmitFunc =
  { name :: String
  , arity :: Int
  , paramTys :: Array Ty
  , retTy :: Ty
  , registerCount :: Int
  , body :: Array MInstr
  , isEntry :: Boolean
  }

type Pool = Array ValueVM

addConst :: ValueVM -> State Pool Int
addConst v = do
  pool <- get
  case Array.findIndex (_ == v) pool of
    Just i -> pure i
    Nothing -> do
      put (Array.snoc pool v)
      pure (Array.length pool)

litToValue :: Lit -> ValueVM
litToValue = case _ of
  LUnit -> VUnitVM
  LInt s -> VIntVM s
  LFixed v sc -> VFixedVM v sc
  LRational n d ->
    let r = Rational.reduce n d
    in VRationalVM r.num r.den
  LBool b -> VBoolVM b
  LStr s -> VStringVM s

tyToVM :: Ty -> TypeVM
tyToVM = case _ of
  TInt -> TyInt
  TFixed -> TyFixed
  TRational -> TyRational
  TBool -> TyBool
  TString -> TyString
  TUnit -> TyUnit
  TPid -> TyUnit
  TList _ -> TyList
  TRecord _ -> TyRecord
  TData _ _ -> TyRecord
  TVar _ -> TyUnit
  TArrow _ _ -> TyUnit
  TUnknown -> TyUnit

convInstr :: MInstr -> State Pool InstructionVM
convInstr = case _ of
  MLoad d lit -> do
    i <- addConst (litToValue lit)
    pure (LoadConst d i)
  MMove d s -> pure (Move d s)
  MBin op d a b -> pure case op of
    OpAdd -> Add d a b
    OpSub -> Sub d a b
    OpMul -> Mul d a b
    OpDiv -> Div d a b
    OpMod -> Mod d a b
  MCmp op d a b -> pure case op of
    CmpEq -> EqI d a b
    CmpLt -> LtI d a b
    CmpGt -> GtI d a b
  MCall d n args -> pure (Call d n args)
  MSpawn d n args -> pure (Spawn d n args)
  MSend p m -> pure (Send p m)
  MRecv d -> pure (Recv d)
  MYield -> pure Yield
  MSelf d -> pure (Self d)
  MTailCall n args -> pure (TailCall n args)
  MBuiltin d n args -> pure (CallBuiltin d n args)
  MLoadInput d path -> pure (LoadInput d path)
  MEffectNew d typ payload -> pure (EffectNew d typ payload)
  MEffectRequest intent -> pure (EffectRequest intent)
  MEffectAwait intent -> pure (EffectAwait intent)
  MVariantPayload d src -> pure (VariantPayload d src)
  MEffectBatchNew d -> pure (EffectBatchNew d)
  MEffectBatchAppend d batch effect -> pure (EffectBatchAppend d batch effect)
  MRecordNew d -> pure (RecordNew d)
  MRecordSet d r fld v -> pure (RecordSet d r fld v)
  MRecordGet d r fld -> pure (RecordGet d r fld)
  MListNew d -> pure (ListNew d)
  MListAppend d l v -> pure (ListAppend d l v)
  MListGet d l i -> pure (ListGet d l i)
  MListLength d l -> pure (ListLength d l)
  MJump lbl -> pure (Jump lbl)
  MJumpIfFalse c lbl -> pure (JumpIfFalse c lbl)
  MLabel lbl -> pure (Label lbl)
  MRet r -> pure (Return r)
  MHalt r -> pure (Halt r)

convFunc :: EmitFunc -> State Pool FunctionVM
convFunc f = do
  instrs <- traverse convInstr f.body
  pure
    { id: f.name
    , arity: f.arity
    , registerCount: f.registerCount
    , parameterTypes: map tyToVM f.paramTys
    , returnType: tyToVM f.retTy
    , instructions: instrs
    , debug: { name: f.name }
    , proof: { isInvariant: false }
    }

assemble :: Array EmitFunc -> String -> Array InputDecl -> ProgramVM
assemble funcs entry inputDecls =
  let
    Tuple vmFuncs pool = runState (traverse convFunc funcs) []
  in
    { version: "1.0"
    , constants: pool
    , functions: FO.fromFoldable (map (\fn -> Tuple fn.id fn) vmFuncs)
    , stateMachines: FO.empty
    , entrypoint: entry
    , exports: FO.singleton entry entry
    , metadata: { description: "Compiled by Verdict (MIR pipeline)" }
    , typeTable: FO.empty
    , capabilities: inferCapabilities funcs
    , verification: { verified: true }
    -- FinVM defaults maxSteps to 10000, which deep recursion blows past; emit a
    -- generous bound (deterministic programs still halt; this only caps runaways).
    , limits: { maxSteps: 100000000 }
    , inputs: inputsSchema inputDecls
    }

inputsSchema :: Array InputDecl -> Maybe InputsVM
inputsSchema decls =
  if Array.null decls then Nothing
  else
    Just
      ( InputsVM
          { schema:
              map
                (\(InputDecl n t def) ->
                  InputSchemaEntry
                    { name: n
                    , typeName: tyToSchemaName t
                    , required: isNothing def
                    }
                )
                decls
          , defaults: inputDefaults decls
          }
      )

inputDefaults :: Array InputDecl -> FO.Object ValueVM
inputDefaults decls = FO.fromFoldable (Array.mapMaybe defaultEntry decls)
  where
  defaultEntry (InputDecl n _ (Just (ELit lit))) = Just (Tuple n (litToValue lit))
  defaultEntry _ = Nothing

tyToSchemaName :: Ty -> String
tyToSchemaName = case _ of
  TInt -> "Int"
  TFixed -> "Fixed"
  TRational -> "Rational"
  TBool -> "Bool"
  TString -> "String"
  TUnit -> "Unit"
  TPid -> "Pid"
  TList t -> "List " <> tyToSchemaName t
  TRecord _ -> "Record"
  TData n _ -> n
  TVar a -> a
  TArrow _ _ -> "Function"
  TUnknown -> "Json"

-- | Capabilities are the namespaces of the builtins/effects actually invoked
-- | (`"db.insert@1"` / `"db.insert"` -> `"db"`), so a program declares exactly
-- | the host powers it uses (db / cache / bigint / logic / http / sys).
inferCapabilities :: Array EmitFunc -> Array String
inferCapabilities funcs =
  Array.nub (Array.sort (Array.mapMaybe ns (Array.concatMap _.body funcs)))
  where
  ns (MBuiltin _ bid _) = Just (SCU.takeWhile (_ /= '.') bid)
  ns (MEffectNew _ typ _) = Just (SCU.takeWhile (_ /= '.') typ)
  ns _ = Nothing
