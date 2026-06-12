-- | Register allocation: maps unbounded virtual registers to a minimal set of
-- | physical registers via last-use linear scan. Because control flow is
-- | structured (forward branches, one loop back-edge), a value's physical
-- | register can be reused as soon as its last use passes — so `registerCount`
-- | tracks peak live values (≈ expression depth) instead of the number of
-- | subexpressions. Parameters are pinned to registers 0..arity-1 per the ABI.
-- |
-- | A move-coalescing heuristic lets `MMove d s` reuse `s`'s register when `s`
-- | dies at the move, turning the move into a self-move that the peephole drops.
module Verdict.Core.Regalloc (allocate) where

import Prelude

import Data.Array as Array
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Verdict.Core.MIR (MFunc, MInstr(..), VReg, defOf, mapVRegs, regsOf)

type Alloc = { body :: Array MInstr, registerCount :: Int }

type Scan =
  { assign :: Map VReg Int -- vreg -> physical register
  , live :: Set VReg -- temps currently occupying a register (params excluded)
  , free :: Array Int -- free physical registers, ascending
  , next :: Int -- next never-used physical register
  , maxReg :: Int -- highest physical register touched (-1 = none)
  }

allocate :: MFunc -> Alloc
allocate f =
  let
    arity = Array.length f.params
    paramSet = Set.fromFoldable f.params
    lastUse = computeLastUse f.body

    init :: Scan
    init =
      { assign: Map.fromFoldable (Array.mapWithIndex (\i p -> Tuple p i) f.params)
      , live: Set.empty
      , free: []
      , next: arity
      , maxReg: arity - 1
      }

    final = foldlWithIndex (step paramSet lastUse) init f.body
    remap r = fromMaybe r (Map.lookup r final.assign)
    allocated = map (mapVRegs remap) f.body
    cleaned = Array.filter (not <<< isSelfMove) allocated
  in
    { body: cleaned, registerCount: max 1 (final.maxReg + 1) }

-- | Last instruction index at which each vreg appears (def or use).
computeLastUse :: Array MInstr -> Map VReg Int
computeLastUse = foldlWithIndex (\idx m i -> Array.foldl (\acc r -> Map.insert r idx acc) m (regsOf i)) Map.empty

step :: Set VReg -> Map VReg Int -> Int -> Scan -> MInstr -> Scan
step paramSet lastUse idx s0 instr =
  let
    -- 1. Release temps whose last use is already behind us.
    diedAt v = fromMaybe idx (Map.lookup v lastUse) < idx
    died = Array.filter diedAt (Set.toUnfoldable s0.live)
    releasedRegs = Array.mapMaybe (\v -> Map.lookup v s0.assign) died
    s1 = s0
      { live = Array.foldl (flip Set.delete) s0.live died
      , free = Array.sort (s0.free <> releasedRegs)
      }
  in
    -- 2. Allocate (or coalesce) this instruction's definition.
    case instr of
      MMove d src
        | not (Map.member d s1.assign)
        , not (Set.member src paramSet)
        , Set.member src s1.live
        , Map.lookup src lastUse == Just idx ->
            -- `src` dies here: hand its register to `d` (coalesce).
            case Map.lookup src s1.assign of
              Just phys -> s1
                { assign = Map.insert d phys s1.assign
                , live = Set.insert d (Set.delete src s1.live)
                }
              Nothing -> allocDef paramSet d s1
      _ -> case defOf instr of
        Just d -> allocDef paramSet d s1
        Nothing -> s1

-- | Give `d` a register (unless it is a pinned param or already assigned),
-- | preferring a freed one.
allocDef :: Set VReg -> VReg -> Scan -> Scan
allocDef paramSet d s
  | Map.member d s.assign = s
  | Set.member d paramSet = s
  | otherwise = case Array.uncons s.free of
      Just { head, tail } -> s
        { assign = Map.insert d head s.assign
        , live = Set.insert d s.live
        , free = tail
        , maxReg = max s.maxReg head
        }
      Nothing -> s
        { assign = Map.insert d s.next s.assign
        , live = Set.insert d s.live
        , next = s.next + 1
        , maxReg = max s.maxReg s.next
        }

isSelfMove :: MInstr -> Boolean
isSelfMove (MMove d src) = d == src
isSelfMove _ = false
