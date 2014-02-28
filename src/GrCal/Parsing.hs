module Parsing (Expr, Stmt, stmt, Table, table, parseTable) where

import Prelude hiding (elem, lines)
import Text.ParserCombinators.Parsec

data Let = Let deriving Show
data Equ = Equ deriving Show
data In = In deriving Show
data Print = Print deriving Show
newtype Id = Id String deriving Show
data Expr = MemberExpr Id In Id | FunctionExpr [Id] deriving Show
data Stmt = PrintId Print Id | PrintExpr Print Expr | AssignStmt Let Id Equ Expr deriving Show

newtype Elem = Elem Id deriving Show
data TableLine = TableLine [Elem] deriving Show
data Table = Table TableLine [TableLine] deriving Show

-- |This parser just gets the reserved word let
let' :: Parser Let
let' = string "let" >> return Let

-- |This parser just gets the reserved word =
equ :: Parser Equ
equ = string "=" >> return Equ

-- |This parser just gets the reserved word in
in' :: Parser In
in' = string "in" >> return In

-- |This parser just gets the reserved word print
print' :: Parser Print
print' = string "print" >> return Print

-- |This parser constructs an Id, which is a symbol that begins with
-- a letter and consists of all letters and numbers.
idParser :: Parser Id
idParser = do
  first <- letter
  rest <- many (letter <|> digit)
  let id' = first : rest
  return $ Id id'

-- |This parser reads in a membership expression.
memberExprParser :: Parser Expr
memberExprParser = do
  first <- idParser
  spaces
  secondItem <- in'
  spaces
  sourceSet <- idParser
  return $ MemberExpr first secondItem sourceSet

-- |This parser reads in a function call expression.
functionExprParser :: Parser Expr
functionExprParser = do
  func <- many idParser
  return $ FunctionExpr func

-- |This function call composes all expression parsers
expr :: Parser Expr
expr = memberExprParser <|> functionExprParser

-- |This parser reads a print id statement
printId :: Parser Stmt
printId = do
  printLiteral <- print'
  spaces
  targetId <- idParser
  spaces
  return $ PrintId printLiteral targetId

-- |This parser reads a print expression statement
printExpr :: Parser Stmt
printExpr = do
  printLiteral <- print'
  spaces
  targetExpr <- expr
  spaces
  return $ PrintExpr printLiteral targetExpr

-- |This parser reads an assign statement
assignStmt :: Parser Stmt
assignStmt = do
  letLiteral <- let'
  spaces
  assignId <- idParser
  spaces
  eqLiteral <- equ
  spaces
  targetExpr <- expr
  return $ AssignStmt letLiteral assignId eqLiteral targetExpr

-- |This parser is the composition of all statement parsers
stmt :: Parser Stmt
stmt = printId <|> printExpr <|> assignStmt

-- |This parser reads an element of a group table. This is equivalent to
-- reading an Id.
elems :: Parser [Elem]
elems = do
  tempId <- idParser
  remaining <- remainingElems
  return $ Elem tempId : remaining

-- |Mutually recursive parser to retrieve the remaining elements in a table
-- line.
remainingElems :: Parser [Elem]
remainingElems = (char '\t' >> elems) <|> return []

-- |This parser reads a line full of elements of a group
tableLines :: Parser [TableLine]
tableLines = do
  lineElems <- elems
  remaining <- remainingLines
  return $ TableLine lineElems : remaining

remainingLines :: Parser [TableLine]
remainingLines = (char '\n' >> tableLines) <|> return []

-- |This parser reads multiple lines from a table.
table :: Parser Table
table = do
  lines <- tableLines
  return $ Table (head lines) (tail lines)

parseTable :: String -> Either ParseError Table
parseTable = parse table "(unknown)"
