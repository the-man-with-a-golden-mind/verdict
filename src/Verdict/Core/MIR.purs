-- | MIR: a three-address intermediate representation over *virtual* registers
-- | (unbounded, SSA-ish — each is assigned once by lowering). This is the
-- | substrate the optimizer and register allocator work on, sitting between the
-- | AST and FinVM bytecode. Control flow is explicit via labels + branches.
module Verdict.Core.MIR where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Verdict.Syntax.AST (Name, Lit, BinOp, CmpOp, Ty)

type VReg = Int
type Label = String

data MInstr
  = MLoad VReg Lit
  | MMove VReg VReg
  | MBin BinOp VReg VReg VReg
  | MCmp CmpOp VReg VReg VReg
  | MCall VReg Name (Array VReg)
  | MSpawn VReg Name (Array VReg)
  | MSend VReg VReg
  | MRecv VReg
  | MYield
  | MSelf VReg
  -- | A call in tail position: transfers control to `name` reusing the current
  -- | frame (no destination register — the callee's RETURN returns to *our*
  -- | caller). Emitted by the tail-call peephole; maps to FinVM `TAIL_CALL`.
  | MTailCall Name (Array VReg)
  | MBuiltin VReg String (Array VReg)
  | MLoadInput VReg String
  | MEffectNew VReg String VReg
  | MEffectRequest VReg
  | MEffectBatchNew VReg
  | MEffectBatchAppend VReg VReg VReg
  -- | Async effect (FinVM 1.1.0): suspend ONLY this process on the intent's
  -- | correlation key; the host driver delivers the result to the mailbox as a
  -- | `VVariant "EffectReply" { key, value }`, read back via PROC_RECEIVE.
  | MEffectAwait VReg
  -- | Unwrap a variant's payload (FinVM `VARIANT_PAYLOAD`); used to read the
  -- | `{ key, value }` record out of an effect-reply message.
  | MVariantPayload VReg VReg
  | MRecordNew VReg
  | MRecordSet VReg VReg Name VReg
  | MRecordGet VReg VReg Name
  | MListNew VReg
  | MListAppend VReg VReg VReg
  | MListGet VReg VReg VReg
  | MListLength VReg VReg
  | MLabel Label
  | MJump Label
  | MJumpIfFalse VReg Label
  | MRet VReg
  | MHalt VReg

type MFunc =
  { name :: Name
  , params :: Array VReg
  , paramTys :: Array Ty
  , retTy :: Ty
  , body :: Array MInstr
  , isEntry :: Boolean
  }

-- | The virtual register this instruction writes, if any.
defOf :: MInstr -> Maybe VReg
defOf = case _ of
  MLoad d _ -> Just d
  MMove d _ -> Just d
  MBin _ d _ _ -> Just d
  MCmp _ d _ _ -> Just d
  MCall d _ _ -> Just d
  MSpawn d _ _ -> Just d
  MRecv d -> Just d
  MSelf d -> Just d
  MBuiltin d _ _ -> Just d
  MLoadInput d _ -> Just d
  MEffectNew d _ _ -> Just d
  MVariantPayload d _ -> Just d
  MEffectBatchNew d -> Just d
  MEffectBatchAppend d _ _ -> Just d
  MRecordNew d -> Just d
  MRecordSet d _ _ _ -> Just d
  MRecordGet d _ _ -> Just d
  MListNew d -> Just d
  MListAppend d _ _ -> Just d
  MListGet d _ _ -> Just d
  MListLength d _ -> Just d
  _ -> Nothing

-- | The virtual registers this instruction reads.
usesOf :: MInstr -> Array VReg
usesOf = case _ of
  MMove _ s -> [ s ]
  MBin _ _ a b -> [ a, b ]
  MCmp _ _ a b -> [ a, b ]
  MCall _ _ args -> args
  MSpawn _ _ args -> args
  MSend p m -> [ p, m ]
  MTailCall _ args -> args
  MBuiltin _ _ args -> args
  MEffectNew _ _ payload -> [ payload ]
  MEffectRequest intent -> [ intent ]
  MEffectAwait intent -> [ intent ]
  MVariantPayload _ src -> [ src ]
  MEffectBatchAppend _ batch effect -> [ batch, effect ]
  MRecordSet _ r _ v -> [ r, v ]
  MRecordGet _ r _ -> [ r ]
  MListAppend _ l v -> [ l, v ]
  MListGet _ l i -> [ l, i ]
  MListLength _ l -> [ l ]
  MJumpIfFalse c _ -> [ c ]
  MRet r -> [ r ]
  MHalt r -> [ r ]
  _ -> []

-- | Pure instructions are safe to eliminate when their result is unused, and to
-- | deduplicate (CSE). Calls/builtins and the record/list *builders* are treated
-- | as having effects to stay conservative.
isPure :: MInstr -> Boolean
isPure = case _ of
  MLoad _ _ -> true
  MMove _ _ -> true
  MBin _ _ _ _ -> true
  MCmp _ _ _ _ -> true
  MRecordGet _ _ _ -> true
  MListGet _ _ _ -> true
  MListLength _ _ -> true
  _ -> false

-- | Rewrite every virtual register (both defs and uses) through `f`. Used by the
-- | allocator to substitute physical registers.
mapVRegs :: (VReg -> VReg) -> MInstr -> MInstr
mapVRegs f = case _ of
  MLoad d l -> MLoad (f d) l
  MMove d s -> MMove (f d) (f s)
  MBin op d a b -> MBin op (f d) (f a) (f b)
  MCmp op d a b -> MCmp op (f d) (f a) (f b)
  MCall d n args -> MCall (f d) n (map f args)
  MSpawn d n args -> MSpawn (f d) n (map f args)
  MSend p m -> MSend (f p) (f m)
  MRecv d -> MRecv (f d)
  MYield -> MYield
  MSelf d -> MSelf (f d)
  MTailCall n args -> MTailCall n (map f args)
  MBuiltin d n args -> MBuiltin (f d) n (map f args)
  MLoadInput d path -> MLoadInput (f d) path
  MEffectNew d n payload -> MEffectNew (f d) n (f payload)
  MEffectRequest intent -> MEffectRequest (f intent)
  MEffectAwait intent -> MEffectAwait (f intent)
  MVariantPayload d src -> MVariantPayload (f d) (f src)
  MEffectBatchNew d -> MEffectBatchNew (f d)
  MEffectBatchAppend d batch effect -> MEffectBatchAppend (f d) (f batch) (f effect)
  MRecordNew d -> MRecordNew (f d)
  MRecordSet d r fld v -> MRecordSet (f d) (f r) fld (f v)
  MRecordGet d r fld -> MRecordGet (f d) (f r) fld
  MListNew d -> MListNew (f d)
  MListAppend d l v -> MListAppend (f d) (f l) (f v)
  MListGet d l i -> MListGet (f d) (f l) (f i)
  MListLength d l -> MListLength (f d) (f l)
  MJumpIfFalse c lbl -> MJumpIfFalse (f c) lbl
  MRet r -> MRet (f r)
  MHalt r -> MHalt (f r)
  other -> other

-- | All virtual registers mentioned by an instruction (defs ∪ uses).
regsOf :: MInstr -> Array VReg
regsOf i = case defOf i of
  Just d -> Array.cons d (usesOf i)
  Nothing -> usesOf i
