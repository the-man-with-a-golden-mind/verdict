module Verdict.Syntax.AST where

import Prelude

import Data.Foldable (foldl)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)

type Name = String

type SourcePos = { line :: Int, column :: Int }

--------------------------------------------------------------------------------
-- Surface type language
--------------------------------------------------------------------------------

-- | The type language. `TVar` is a (universally quantified) type variable;
-- | `TData n args` is a data type applied to type arguments (`Option Int`).
-- | `TUnknown` is an internal wildcard used by the checker; it is not surface
-- | syntax.
data Ty
  = TInt
  | TFixed
  | TRational
  | TVar Name
  | TData Name (Array Ty)
  | TBool
  | TString
  | TUnit
  | TPid
  | TList Ty
  | TRecord (Array (Tuple Name Ty))
  | TArrow Ty Ty
  | TUnknown

derive instance eqTy :: Eq Ty
instance showTy :: Show Ty where
  show TInt = "Int"
  show TFixed = "Fixed"
  show TRational = "Rational"
  show (TVar a) = a
  show (TData n args) = foldl (\acc t -> acc <> " " <> show t) n args
  show TBool = "Bool"
  show TString = "String"
  show TUnit = "Unit"
  show TPid = "Pid"
  show (TList t) = "(List " <> show t <> ")"
  show (TRecord fs) = "{" <> show fs <> "}"
  show (TArrow a b) = "(" <> show a <> " -> " <> show b <> ")"
  show TUnknown = "?"

-- | Number of leading arrows (the arity a function type promises).
typeArity :: Ty -> Int
typeArity (TArrow _ b) = 1 + typeArity b
typeArity _ = 0

-- | Split an arrow type into (paramTypes, resultType), taking at most `n`
-- | parameters off the front.
splitArrow :: Int -> Ty -> { params :: Array Ty, result :: Ty }
splitArrow n t
  | n <= 0 = { params: [], result: t }
  | otherwise = case t of
      TArrow a b ->
        let rest = splitArrow (n - 1) b
        in { params: [ a ] <> rest.params, result: rest.result }
      _ -> { params: [], result: t }

--------------------------------------------------------------------------------
-- Operators
--------------------------------------------------------------------------------

data BinOp = OpAdd | OpSub | OpMul | OpDiv | OpMod

derive instance eqBinOp :: Eq BinOp
instance showBinOp :: Show BinOp where
  show OpAdd = "+"
  show OpSub = "-"
  show OpMul = "*"
  show OpDiv = "/"
  show OpMod = "mod"

data CmpOp = CmpEq | CmpLt | CmpGt

derive instance eqCmpOp :: Eq CmpOp
instance showCmpOp :: Show CmpOp where
  show CmpEq = "=="
  show CmpLt = "<"
  show CmpGt = ">"

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

-- | Numeric literals keep their textual form so arbitrary precision survives.
-- | `LFixed digits scale` represents `digits * 10^-scale` (e.g. "150" scale 2 = 1.50).
data Lit
  = LInt String
  | LFixed String Int
  | LRational String String
  | LUnit
  | LBool Boolean
  | LStr String

derive instance eqLit :: Eq Lit
instance showLit :: Show Lit where
  show (LInt s) = s
  show (LFixed s n) = "Fixed(" <> s <> "," <> show n <> ")"
  show (LRational n d) = n <> "/" <> d
  show LUnit = "unit"
  show (LBool b) = show b
  show (LStr s) = show s

data Expr
  = ELit Lit
  | EAt SourcePos Expr
  | EVar Name
  | EBin BinOp Expr Expr
  | ECmp CmpOp Expr Expr
  | EIf Expr Expr Expr
  | ELet Name Expr Expr
  | ECall Name (Array Expr)
  -- | `builtin("ns.fn@v", ...)` — a PURE host function (CALL_BUILTIN).
  | EBuiltin String (Array Expr)
  -- | `effect("ns.fn@v", ...)` — an ASYNC host effect (EFFECT_AWAIT protocol), for
  -- | any namespace. Lets Verdict programs add effectful FFI without a compiler or
  -- | VM release: the host just registers a handler for the effect type.
  | EEffect String (Array Expr)
  | EList (Array Expr)
  | ERecord (Array (Tuple Name Expr))
  | EField Expr Name
  -- | Explicit multiway switch on a value. Arms match a literal; `Nothing` is
  -- | the required `default` arm. This is value-matching, not destructuring.
  | ESwitch Expr (Array (Tuple (Maybe Lit) Expr))
  | EMatch Expr (Array (Tuple Pattern Expr))

stripAt :: Expr -> Expr
stripAt = case _ of
  EAt _ e -> stripAt e
  e -> e

data Pattern
  = PCtor Name (Array Name)
  | PWild

derive instance eqPattern :: Eq Pattern
instance showPattern :: Show Pattern where
  show PWild = "_"
  show (PCtor n xs) = n <> " " <> show xs

--------------------------------------------------------------------------------
-- Declarations & modules
--------------------------------------------------------------------------------

type Ctor = { name :: Name, fields :: Array Ty }

-- | `TypeDecl name typeParams ctors` — e.g. `type Option a = Some a | None` is
-- | `TypeDecl "Option" ["a"] [...]`. Ctor field types may mention the params.
data TypeDecl = TypeDecl Name (Array Name) (Array Ctor)

-- | A top-level declaration. Signatures are required (bidirectional checking),
-- | but the parser tolerates their absence so the checker can report it.
newtype Decl = Decl
  { name :: Name
  , params :: Array Name
  , sig :: Maybe Ty
  , body :: Expr
  }

declName :: Decl -> Name
declName (Decl d) = d.name

data Module = Module Name (Array TypeDecl) (Array Decl)

moduleDecls :: Module -> Array Decl
moduleDecls (Module _ _ ds) = ds

moduleName :: Module -> Name
moduleName (Module n _ _) = n

moduleTypes :: Module -> Array TypeDecl
moduleTypes (Module _ ts _) = ts

-- | A module's export list (`exposing (..)` or `exposing (a, B, …)`).
data Exposing = ExposeAll | ExposeNames (Array Name)

-- | `import Foo exposing (bar, Baz)`.
type Import = { mod :: Name, names :: Exposing }

-- | The parser's full output for one module: the core `Module` (name + decls +
-- | type decls, which is all the downstream pipeline needs) plus its export list
-- | and imports, used by multi-file resolution.
type ParsedModule = { mod :: Module, exposing :: Exposing, imports :: Array Import }
