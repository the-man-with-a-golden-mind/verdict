module Verdict.FinVM.Types where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as J
import Data.Array as Array
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Combinators ((:=), (~>))
import Foreign.Object as FO

--------------------------------------------------------------------------------
-- Values (constant pool)
--------------------------------------------------------------------------------

-- | BigInt-ish payloads are carried as decimal strings so precision is exact.
data ValueVM
  = VUnitVM
  | VIntVM String
  | VFixedVM String Int
  | VRationalVM String String
  | VBoolVM Boolean
  | VStringVM String

derive instance eqValueVM :: Eq ValueVM

-- | Tagless single-key encoding, per the FinVM JSON spec:
-- | VInt -> {"int":"42"}, VFixed -> {"fixed":{"value","scale"}},
-- | VBool -> {"bool":true}, VString -> {"string":"hi"}.
instance encodeJsonValueVM :: EncodeJson ValueVM where
  encodeJson VUnitVM =
    J.jsonNull
  encodeJson (VIntVM s) =
    "int" := s ~> J.jsonEmptyObject
  encodeJson (VFixedVM v sc) =
    "fixed" := ("value" := v ~> "scale" := sc ~> J.jsonEmptyObject)
      ~> J.jsonEmptyObject
  encodeJson (VRationalVM n d) =
    "rational" := ("numerator" := n ~> "denominator" := d ~> J.jsonEmptyObject)
      ~> J.jsonEmptyObject
  encodeJson (VBoolVM b) =
    "bool" := b ~> J.jsonEmptyObject
  encodeJson (VStringVM s) =
    "string" := s ~> J.jsonEmptyObject

--------------------------------------------------------------------------------
-- Types (parameterTypes / returnType metadata)
--------------------------------------------------------------------------------

data TypeVM
  = TyInt
  | TyFixed
  | TyRational
  | TyBool
  | TyString
  | TyUnit
  | TyList
  | TyRecord

instance encodeJsonTypeVM :: EncodeJson TypeVM where
  encodeJson t = "tag" := tag ~> J.jsonEmptyObject
    where
    tag = case t of
      TyInt -> "TInt"
      TyFixed -> "TFixed"
      TyRational -> "TRational"
      TyBool -> "TBool"
      TyString -> "TString"
      TyUnit -> "TUnit"
      TyList -> "TList"
      TyRecord -> "TRecord"

--------------------------------------------------------------------------------
-- Instructions
--------------------------------------------------------------------------------

data InstructionVM
  = LoadConst Int Int
  | Move Int Int
  | Add Int Int Int
  | Sub Int Int Int
  | Mul Int Int Int
  | Div Int Int Int
  | Mod Int Int Int
  | EqI Int Int Int
  | LtI Int Int Int
  | GtI Int Int Int
  | Call Int String (Array Int)
  | TailCall String (Array Int)
  | CallBuiltin Int String (Array Int)
  | Spawn Int String (Array Int)
  | Send Int Int
  | Recv Int
  | Yield
  | Self Int
  | Jump String
  | JumpIfFalse Int String
  | Label String
  | Return Int
  | Halt Int
  | RecordNew Int
  | RecordSet Int Int String Int
  | RecordGet Int Int String
  | ListNew Int
  | ListAppend Int Int Int
  | ListGet Int Int Int
  | ListLength Int Int

-- | Positional encoding, per the FinVM JSON spec: every instruction is a flat
-- | JSON array ["OPCODE", ...args] (NOT a {tag,contents} object). `ints` covers
-- | the all-register opcodes; `hetero` covers those mixing registers with
-- | strings / arg arrays (and DIV's rounding-mode string at index 2).
instance encodeJsonInstructionVM :: EncodeJson InstructionVM where
  encodeJson = case _ of
    LoadConst dst i -> ints "LOAD_CONST" [ dst, i ]
    Move dst src -> ints "MOVE" [ dst, src ]
    Add d a b -> ints "ADD" [ d, a, b ]
    Sub d a b -> ints "SUB" [ d, a, b ]
    Mul d a b -> ints "MUL" [ d, a, b ]
    EqI d a b -> ints "EQ" [ d, a, b ]
    LtI d a b -> ints "LT" [ d, a, b ]
    GtI d a b -> ints "GT" [ d, a, b ]
    Return r -> ints "RETURN" [ r ]
    Halt r -> ints "HALT" [ r ]
    RecordNew d -> ints "RECORD_NEW" [ d ]
    ListNew d -> ints "LIST_NEW" [ d ]
    ListAppend d l v -> ints "LIST_APPEND" [ d, l, v ]
    ListGet d l i -> ints "LIST_GET" [ d, l, i ]
    ListLength d l -> ints "LIST_LENGTH" [ d, l ]
    Mod d a b -> ints "MOD" [ d, a, b ]
    Div d a b -> hetero "DIV" [ encodeJson d, encodeJson "RoundDown", encodeJson a, encodeJson b ]
    Call d fid args -> hetero "CALL" [ encodeJson d, encodeJson fid, encodeJson args ]
    TailCall fid args -> hetero "TAIL_CALL" [ encodeJson fid, encodeJson args ]
    CallBuiltin d bid args -> hetero "CALL_BUILTIN" [ encodeJson d, encodeJson bid, encodeJson args ]
    Spawn d fid args -> hetero "PROC_SPAWN" [ encodeJson d, encodeJson fid, encodeJson args ]
    Send p m -> ints "PROC_SEND" [ p, m ]
    Recv d -> ints "PROC_RECEIVE" [ d ]
    Yield -> ints "PROC_YIELD" []
    Self d -> ints "PROC_SELF" [ d ]
    Jump lbl -> hetero "JUMP" [ encodeJson lbl ]
    JumpIfFalse c lbl -> hetero "JUMP_IF_FALSE" [ encodeJson c, encodeJson lbl ]
    Label lbl -> hetero "LABEL" [ encodeJson lbl ]
    RecordSet d r fld v -> hetero "RECORD_SET" [ encodeJson d, encodeJson r, encodeJson fld, encodeJson v ]
    RecordGet d r fld -> hetero "RECORD_GET" [ encodeJson d, encodeJson r, encodeJson fld ]

ints :: String -> Array Int -> Json
ints op contents = J.fromArray (J.fromString op `Array.cons` map encodeJson contents)

hetero :: String -> Array Json -> Json
hetero op contents = J.fromArray (J.fromString op `Array.cons` contents)

--------------------------------------------------------------------------------
-- Functions & program
--------------------------------------------------------------------------------

type FunctionVM =
  { id :: String
  , arity :: Int
  , registerCount :: Int
  , parameterTypes :: Array TypeVM
  , returnType :: TypeVM
  , instructions :: Array InstructionVM
  , debug :: { name :: String }
  , proof :: { isInvariant :: Boolean }
  }

encodeFunctionVM :: FunctionVM -> Json
encodeFunctionVM f =
  "proof" := f.proof
    ~> "instructions" := f.instructions
    ~> "registerCount" := f.registerCount
    ~> "arity" := f.arity
    ~> J.jsonEmptyObject

type ProgramVM =
  { version :: String
  , constants :: Array ValueVM
  , functions :: FO.Object FunctionVM
  , stateMachines :: FO.Object J.Json
  , entrypoint :: String
  , exports :: FO.Object String
  , metadata :: { description :: String }
  , typeTable :: FO.Object J.Json
  , capabilities :: Array String
  , verification :: { verified :: Boolean }
  , limits :: { maxSteps :: Int }
  }

encodeProgramVM :: ProgramVM -> Json
encodeProgramVM p =
  "capabilities" := p.capabilities
    ~> "limits" := ("maxSteps" := p.limits.maxSteps ~> J.jsonEmptyObject)
    ~> "functions" := map encodeFunctionVM p.functions
    ~> "entrypoint" := p.entrypoint
    ~> "constants" := p.constants
    ~> "version" := p.version
    ~> J.jsonEmptyObject
