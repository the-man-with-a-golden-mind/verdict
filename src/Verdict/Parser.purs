module Verdict.Parser (parseVerdict, parseModuleFull) where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (defer)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as SC
import Data.Tuple (Tuple(..))
import Parsing (ParseError, Parser, Position(..), fail, position, runParser)
import Parsing.Combinators (between, chainl1, notFollowedBy, optionMaybe, option, sepBy, try)
import Parsing.Combinators.Array as PA
import Parsing.Language (emptyDef)
import Parsing.String (char, eof, string)
import Parsing.String.Basic (alphaNum, digit, lower, upper)
import Parsing.Token (GenLanguageDef(..), LanguageDef, TokenParser, makeTokenParser, unGenLanguageDef)
import Verdict.Syntax.AST (BinOp(..), Ctor, CmpOp(..), Decl(..), Exposing(..), Expr(..), Import, InputDecl(..), Lit(..), Module(..), Name, ParsedModule, Pattern(..), Ty(..), TypeDecl(..))

-- | `emptyDef` with Haskell-style comments enabled (`--` line, `{- -}` block).
langDef :: LanguageDef
langDef = LanguageDef (unGenLanguageDef emptyDef)
  { commentLine = "--"
  , commentStart = "{-"
  , commentEnd = "-}"
  , nestedComments = true
  }

tokenParser :: TokenParser
tokenParser = makeTokenParser langDef

identifier :: Parser String String
identifier = tokenParser.identifier

typeIdentifier :: Parser String String
typeIdentifier = tokenParser.lexeme do
  c <- upper
  rest <- PA.many alphaNum
  pure (SC.fromCharArray (c `Array.cons` rest))

-- | A lowercase-initial identifier, used for type variables (`a`, `acc`).
lowerIdentifier :: Parser String String
lowerIdentifier = tokenParser.lexeme do
  c <- lower
  rest <- PA.many alphaNum
  pure (SC.fromCharArray (c `Array.cons` rest))

symbol :: String -> Parser String String
symbol = tokenParser.symbol

stringLiteral :: Parser String String
stringLiteral = tokenParser.stringLiteral

-- | A boundary-checked keyword (not a prefix of a longer identifier).
keyword :: String -> Parser String Unit
keyword k = tokenParser.lexeme (try (string k <* notFollowedBy alphaNum)) $> unit

digits :: Parser String String
digits = (SC.fromCharArray <<< NEA.toArray) <$> PA.many1 digit

-- | Numeric literal: integer (`42`), fixed-point decimal (`1.50` -> value 150,
-- | scale 2), or rational (`1 % 2`). Negative allowed via leading `-`.
numberLit :: Parser String Lit
numberLit = tokenParser.lexeme do
  sign <- option "" (string "-")
  intPart <- digits
  fixedLit sign intPart
    <|> try (rationalLit sign intPart)
    <|> pure (LInt (sign <> intPart))
  where
  fixedLit sign intPart = do
    fs <- char '.' *> digits
    pure (LFixed (sign <> intPart <> fs) (SC.length fs))

  rationalLit sign intPart = do
    tokenParser.whiteSpace
    _ <- symbol "%"
    den <- digits
    pure (LRational (sign <> intPart) den)

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

parseExpr :: Parser String Expr
parseExpr = defer \_ -> parseIf <|> parseLet <|> parseSwitch <|> parseMatch <|> parseCompare

-- | switch e { 1 -> a, 2 -> b, _ -> c }  — explicit value matching (`_` = default).
parseSwitch :: Parser String Expr
parseSwitch = defer \_ -> do
  keyword "switch"
  scrut <- parseExpr
  _ <- symbol "{"
  arms <- sepBy parseArm (symbol ",")
  _ <- symbol "}"
  pure (ESwitch scrut (Array.fromFoldable arms))
  where
  parseArm = do
    pat <- (symbol "_" $> Nothing) <|> (Just <$> parseLitPat)
    _ <- symbol "->"
    body <- parseExpr
    pure (Tuple pat body)
  parseLitPat =
    try numberLit
      <|> (LStr <$> stringLiteral)
      <|> (keyword "unit" $> LUnit)
      <|> (keyword "True" $> LBool true)
      <|> (keyword "False" $> LBool false)

parseMatch :: Parser String Expr
parseMatch = defer \_ -> do
  keyword "match"
  scrut <- parseExpr
  _ <- symbol "{"
  arms <- sepBy parseArm (symbol ",")
  _ <- symbol "}"
  pure (EMatch scrut (Array.fromFoldable arms))
  where
  parseArm = do
    pat <- parsePattern
    _ <- symbol "->"
    body <- parseExpr
    pure (Tuple pat body)

  parsePattern =
    (symbol "_" $> PWild)
      <|> (PCtor <$> typeIdentifier <*> (Array.fromFoldable <$> PA.many identifier))

parseIf :: Parser String Expr
parseIf = defer \_ -> do
  keyword "if"
  c <- parseExpr
  keyword "then"
  t <- parseExpr
  keyword "else"
  e <- parseExpr
  pure (EIf c t e)

parseLet :: Parser String Expr
parseLet = defer \_ -> do
  keyword "let"
  n <- identifier <|> (symbol "_" $> "_")
  _ <- symbol "="
  v <- parseExpr
  keyword "in"
  b <- parseExpr
  pure (ELet n v b)

parseCompare :: Parser String Expr
parseCompare = defer \_ -> do
  a <- parseAdd
  m <- optionMaybe (Tuple <$> cmpOp <*> parseAdd)
  pure case m of
    Nothing -> a
    Just (Tuple f b) -> f a b

cmpOp :: Parser String (Expr -> Expr -> Expr)
cmpOp =
  (symbol "==" $> ECmp CmpEq)
    <|> (symbol "<" $> ECmp CmpLt)
    <|> (symbol ">" $> ECmp CmpGt)

parseAdd :: Parser String Expr
parseAdd = defer \_ -> chainl1 parseMul addOp

parseMul :: Parser String Expr
parseMul = defer \_ -> chainl1 parsePostfix mulOp

addOp :: Parser String (Expr -> Expr -> Expr)
addOp = (symbol "+" $> EBin OpAdd) <|> (symbol "-" $> EBin OpSub)

mulOp :: Parser String (Expr -> Expr -> Expr)
mulOp = (symbol "*" $> EBin OpMul) <|> (symbol "/" $> EBin OpDiv)

parsePostfix :: Parser String Expr
parsePostfix = defer \_ -> do
  a <- parseAtom
  ops <- PA.many postfixOp
  pure (Array.foldl (\acc f -> f acc) a ops)

postfixOp :: Parser String (Expr -> Expr)
postfixOp = defer \_ ->
  (flip EField <$> (symbol "." *> identifier))
    <|> do
      ix <- between (symbol "[") (symbol "]") parseExpr
      pure \e -> ECall "get" [ e, ix ]

parseAtom :: Parser String Expr
parseAtom = defer \_ -> do
  Position p <- position
  e <-
    parseLiteral
      <|> parseList
      <|> parseRecord
      <|> parseBuiltin
      <|> parseEffect
      <|> parseCallOrVar
      <|> between (symbol "(") (symbol ")") parseExpr
  pure (EAt { line: p.line, column: p.column } e)

parseLiteral :: Parser String Expr
parseLiteral = ELit <$>
  ( try numberLit
      <|> (LStr <$> stringLiteral)
      <|> (keyword "unit" $> LUnit)
      <|> (keyword "True" $> LBool true)
      <|> (keyword "False" $> LBool false)
  )

parseList :: Parser String Expr
parseList = defer \_ ->
  (EList <<< Array.fromFoldable)
    <$> between (symbol "[") (symbol "]") (sepBy parseExpr (symbol ","))

parseRecord :: Parser String Expr
parseRecord = defer \_ ->
  (ERecord <<< Array.fromFoldable)
    <$> between (symbol "{") (symbol "}") (sepBy recField (symbol ","))
  where
  recField = do
    n <- identifier
    _ <- symbol "="
    e <- parseExpr
    pure (Tuple n e)

parseBuiltin :: Parser String Expr
parseBuiltin = defer \_ -> do
  keyword "builtin"
  _ <- symbol "("
  bid <- stringLiteral
  args <- PA.many (symbol "," *> parseExpr)
  _ <- symbol ")"
  pure (EBuiltin bid args)

-- | `effect("ns.fn@v", arg1, arg2)` — an async host effect (any namespace).
parseEffect :: Parser String Expr
parseEffect = defer \_ -> do
  keyword "effect"
  _ <- symbol "("
  eid <- stringLiteral
  args <- PA.many (symbol "," *> parseExpr)
  _ <- symbol ")"
  pure (EEffect eid args)

parseCallOrVar :: Parser String Expr
parseCallOrVar = defer \_ -> do
  n <- identifier
  margs <- optionMaybe
    (between (symbol "(") (symbol ")") (sepBy parseExpr (symbol ",")))
  pure case margs of
    Nothing -> EVar n
    Just as -> ECall n (Array.fromFoldable as)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

parseType :: Parser String Ty
parseType = defer \_ -> do
  a <- parseTypeApp
  m <- optionMaybe (symbol "->" *> parseType)
  pure case m of
    Nothing -> a
    Just b -> TArrow a b

-- | A type-constructor application: `List X` (built-in), or a user data type
-- | applied to atom arguments (`Option a`, `Result e a`). Primitive type
-- | keywords are matched before a user constructor so `Int` etc. don't parse as
-- | `TData`.
parseTypeApp :: Parser String Ty
parseTypeApp = defer \_ ->
  primitiveType
    <|> (keyword "List" *> (TList <$> parseTypeAtom))
    <|> (TData <$> typeIdentifier <*> (Array.fromFoldable <$> PA.many (guardSameDecl *> parseTypeAtom)))
    <|> parseTypeAtom

-- | Lightweight layout: succeeds only when the next token is NOT at column 1.
-- | Top-level declarations start at column 1; their type-constructor argument
-- | lists and constructor field lists sit on the same line or indented under
-- | them. Without this guard, a bare type variable (`Option a`, `Some a`) would
-- | let `many` run past the newline and swallow the next declaration.
guardSameDecl :: Parser String Unit
guardSameDecl = do
  Position p <- position
  if p.column == 1 then fail "declaration boundary" else pure unit

primitiveType :: Parser String Ty
primitiveType =
  (keyword "Int" $> TInt)
    <|> (keyword "Fixed" $> TFixed)
    <|> (keyword "Rational" $> TRational)
    <|> (keyword "Bool" $> TBool)
    <|> (keyword "String" $> TString)
    <|> (keyword "Unit" $> TUnit)
    <|> (keyword "Pid" $> TPid)
    -- `Json` is the dynamic type for FFI / DB payloads (internally a wildcard).
    <|> (keyword "Json" $> TUnknown)

-- | An atomic type (an application argument): a primitive, a record, a
-- | parenthesised type, a bare data constructor, or a lowercase type variable.
-- | Nested applications must be parenthesised (`Box (Option a)`).
parseTypeAtom :: Parser String Ty
parseTypeAtom = defer \_ ->
  primitiveType
    <|> parseRecordType
    <|> between (symbol "(") (symbol ")") parseType
    <|> ((\n -> TData n []) <$> typeIdentifier)
    <|> (TVar <$> lowerIdentifier)

parseRecordType :: Parser String Ty
parseRecordType = defer \_ ->
  (TRecord <<< Array.fromFoldable)
    <$> between (symbol "{") (symbol "}") (sepBy recField (symbol ","))
  where
  recField = do
    n <- identifier
    _ <- symbol ":"
    t <- parseType
    pure (Tuple n t)

--------------------------------------------------------------------------------
-- Declarations & module
--------------------------------------------------------------------------------

parseTypeDecl :: Parser String TypeDecl
parseTypeDecl = do
  keyword "type"
  name <- typeIdentifier
  params <- PA.many lowerIdentifier
  _ <- symbol "="
  _ <- optionMaybe (symbol "|")
  first <- parseCtor
  rest <- PA.many (symbol "|" *> parseCtor)
  pure (TypeDecl name (Array.fromFoldable params) ([ first ] <> Array.fromFoldable rest))
  where
  parseCtor :: Parser String Ctor
  parseCtor = do
    name <- typeIdentifier
    fields <- PA.many parseCtorField
    pure { name, fields: Array.fromFoldable fields }

  -- A constructor field, bounded by the same column-1 layout rule as type-app
  -- arguments (see `guardSameDecl`) so the field list can't cross into the next
  -- top-level declaration.
  parseCtorField :: Parser String Ty
  parseCtorField = guardSameDecl *> parseTypeApp

parseSig :: Parser String { name :: Name, ty :: Ty }
parseSig = do
  n <- identifier
  _ <- symbol ":"
  t <- parseType
  pure { name: n, ty: t }

parseDef :: Parser String { name :: Name, params :: Array Name, body :: Expr }
parseDef = do
  n <- identifier
  ps <- PA.many identifier
  _ <- symbol "="
  e <- parseExpr
  pure { name: n, params: ps, body: e }

parseInputDecl :: Parser String InputDecl
parseInputDecl = do
  keyword "input"
  n <- identifier
  _ <- symbol ":"
  t <- parseType
  mdef <- optionMaybe (symbol "=" *> parseLiteral)
  pure (InputDecl n t mdef)

parseItem :: Parser String Decl
parseItem = do
  msig <- optionMaybe (try parseSig)
  def <- parseDef
  pure $ Decl
    { name: def.name
    , params: def.params
    , sig: map _.ty msig
    , body: def.body
    }

parseModule :: Parser String ParsedModule
parseModule = do
  tokenParser.whiteSpace
  keyword "module"
  modName <- identifier
  keyword "exposing"
  exposing <- parseExposing
  imports <- Array.fromFoldable <$> PA.many parseImport
  types <- PA.many parseTypeDecl
  inputs <- PA.many parseInputDecl
  items <- PA.many1 parseItem
  _ <- eof
  pure
    { mod: Module modName (Array.fromFoldable types) (Array.fromFoldable inputs) (NEA.toArray items)
    , exposing
    , imports
    }

-- | `( .. )` exposes everything; `( a, B, … )` exposes the listed names.
parseExposing :: Parser String Exposing
parseExposing = between (symbol "(") (symbol ")")
  ( (symbol ".." $> ExposeAll)
      <|> (ExposeNames <<< Array.fromFoldable <$> sepBy exposeName (symbol ","))
  )
  where
  exposeName = identifier <|> typeIdentifier

parseImport :: Parser String Import
parseImport = do
  keyword "import"
  m <- identifier
  keyword "exposing"
  ex <- parseExposing
  pure { mod: m, names: ex }

parseModuleFull :: String -> Either ParseError ParsedModule
parseModuleFull src = runParser src parseModule

parseVerdict :: String -> Either ParseError Module
parseVerdict src = map _.mod (parseModuleFull src)
