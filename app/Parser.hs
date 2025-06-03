{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Parser where

import Ast
import Control.Monad.Combinators.Expr
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Debug as Debug

type Parser = Parsec Void Text

-- Lexer combinators
skipSpace :: Parser ()
skipSpace = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme skipSpace

symbol :: Text -> Parser Text
symbol = L.symbol skipSpace

-- Keywords
kw :: Text -> Parser Text
kw w = string w <* notFollowedBy alphaNumChar <* skipSpace

-- Basic tokens
semi :: Parser Text
semi = symbol ";"

comma :: Parser Text
comma = symbol ","

lbrace :: Parser Text
lbrace = symbol "{"

rbrace :: Parser Text
rbrace = symbol "}"

lparen :: Parser Text
lparen = symbol "("

rparen :: Parser Text
rparen = symbol ")"

lbracket :: Parser Text
lbracket = symbol "["

rbracket :: Parser Text
rbracket = symbol "]"

-- Reserved words
rword :: Text -> Parser Text
rword = kw

-- Identifiers
identifier :: Parser String
identifier = lexeme $ do
  c <- letterChar
  cs <- many (alphaNumChar <|> char '_')
  return (c : cs)

-- Literals
integer :: Parser Int
integer = lexeme L.decimal

stringLiteral :: Parser String
stringLiteral = lexeme $ char '"' *> manyTill L.charLiteral (char '"')

-- Types
parseType :: Parser OatType
parseType = Debug.dbg "parseType" $ do
    core <- Debug.dbg "coreType" $ choice
        [ TBool <$ rword "bool"
        , TInt <$ rword "int"
        , TRef RString <$ rword "string"
        , TRef <$> (RFun <$> (lparen *> sepBy parseType comma <* rparen) <*> (symbol "->" *> parseType))
        ]
    Debug.dbg "arraySuffix" $
      foldl (\t _ -> TRef (RArray t)) core <$> many (lbracket *> rbracket <* skipSpace)


parseRefType :: Parser RefType
parseRefType = Debug.dbg "parseRefType" $
  choice
    [ try $ RString <$ rword "string",
      try $ RArray <$> (parseType <* lbracket <* rbracket),
      RFun <$> (lparen *> sepBy parseType comma <* rparen) <*> (symbol "->" *> parseType)
    ]

parseNullRefType :: Parser RefType
parseNullRefType = Debug.dbg "parseNullRefType" $ do
  ot <- parseType
  case ot of
    TRef rt -> return rt
    _       -> fail "CNull: 'null' must be followed by a reference type (string, array, or function)"

-- Expressions
parseExp :: Parser Exp
parseExp = Debug.dbg "parseExp" $ makeExprParser parseTerm operatorTable

parsePrimaryExp :: Parser Exp
parsePrimaryExp = Debug.dbg "parsePrimaryExp" $ choice
    [ CNull <$> (rword "null" *> parseNullRefType)
    , CBool True <$ rword "true"
    , CBool False <$ rword "false"
    , CInt <$> integer
    , CStr <$> stringLiteral
    , Id <$> identifier
    , parseArrayLiteral
    , parseNewArray
    , parseNewArrayInit
    , parseLength
    , parseParens
    ]

parseTerm :: Parser Exp
parseTerm = Debug.dbg "parseTermLoop" $ do
    e <- parsePrimaryExp
    let loop currentExp = do
            mPostfix <- optional $ choice
                [ try (parseIndexSuffix currentExp)
                , try (parseCallSuffix currentExp)
                ]
            case mPostfix of
                Nothing      -> return currentExp
                Just nextExp -> loop nextExp
    loop e

parseIndexSuffix :: Exp -> Parser Exp
parseIndexSuffix e1 = Debug.dbg "parseIndexSuffix" $ do
    lbracket
    e2 <- parseExp
    rbracket
    return (Index e1 e2)

parseCallSuffix :: Exp -> Parser Exp
parseCallSuffix e = Debug.dbg "parseCallSuffix" $ do
    lparen
    es <- sepBy parseExp comma
    rparen
    return (Call e es)

parseArrayLiteral :: Parser Exp
parseArrayLiteral = Debug.dbg "parseArrayLiteral" $ do
  rword "new"
  t <- parseType
  lbracket
  rbracket
  lbrace
  es <- sepBy parseExp comma
  rbrace
  return $ CArr t es


parseNewArray :: Parser Exp
parseNewArray = do
  rword "new"
  t <- parseType
  lbracket
  e <- parseExp
  rbracket
  return $ NewArr t e

parseNewArrayInit :: Parser Exp
parseNewArrayInit = do
  rword "new"
  t <- parseType
  lbracket
  e1 <- parseExp
  rbracket
  lbrace
  u <- identifier
  symbol "->"
  e2 <- parseExp
  rbrace
  return $ NewArrInit t e1 u e2

parseIndex :: Parser Exp
parseIndex = try $ do
  e1 <- parseExp
  lbracket
  e2 <- parseExp
  rbracket
  return $ Index e1 e2

parseLength :: Parser Exp
parseLength = do
  rword "length"
  lparen
  e <- parseExp
  rparen
  return $ Length e

parseCall :: Parser Exp
parseCall = Debug.dbg "parseCall" $ do
  e <- parseExp
  lparen
  es <- sepBy parseExp comma
  rparen
  return $ Call e es

parseParens :: Parser Exp
parseParens = Debug.dbg "parseParens" $ between lparen rparen parseExp

-- Operators
operatorTable :: [[Operator Parser Exp]]
operatorTable =
  [ [binary "*" Mul],
    [ binary "+" Add,
      binary "-" Sub
    ],
    [ binary "==" Eq,
      binary "!=" Neq,
      binary "<" Lt,
      binary "<=" Lte,
      binary ">" Gt,
      binary ">=" Gte
    ],
    [ binary "&" And,
      binary "|" Or
    ]
  ]

binary :: Text -> BinOp -> Operator Parser Exp
binary name op = InfixL (Bop op <$ symbol name)

-- Statements
parseStmt :: Parser Stmt
parseStmt = do
  notFollowedBy rbrace <?> "statement"
  choice
    [ parseIf
    , parseWhile
    , parseRet
    , parseAssn
    , parseVarDecl
    ]

parseLhs :: Parser Exp
parseLhs = Debug.dbg "parseLhs" $
  choice
    [ Id <$> identifier,
      do
        e1 <- parseExp
        lbracket
        e2 <- parseExp
        rbracket
        return $ Index e1 e2
    ]

parseAssn :: Parser Stmt
parseAssn = Debug.dbg "parseAssn" $ do
  e1 <- parseLhs
  symbol "="
  e2 <- parseExp
  semi
  return $ Assn e1 e2

parseVarDecl :: Parser Stmt
parseVarDecl = Debug.dbg "parseVarDecl" $ do
  rword "var"
  t <- parseType
  ident <- identifier
  symbol "="
  e <- parseExp
  semi
  return $ Decl t ident (Just e)

parseRet :: Parser Stmt
parseRet = do
  rword "return"
  e <- optional parseExp
  semi
  return $ Ret e

parseSCall :: Parser Stmt
parseSCall = do
  e <- parseExp
  lparen
  es <- sepBy parseExp comma
  rparen
  semi
  return $ SCall e es

parseIf :: Parser Stmt
parseIf = Debug.dbg "parseIf" $ do
  rword "if"
  lparen
  e <- parseExp
  rparen
  b1 <- parseBlock
  If e b1 <$> parseElse

parseElse :: Parser [Stmt]
parseElse = Debug.dbg "parseElse" $
  choice
    [ [] <$ rword "else" <* parseBlock,
      [] <$ rword "else" <* parseIf,
      return []
    ]

parseWhile :: Parser Stmt
parseWhile = do
  rword "while"
  lparen
  e <- parseExp
  rparen
  While e <$> parseBlock

parseFor :: Parser Stmt
parseFor = do
  rword "for"
  lparen
  ds <- sepBy parseVarDecl comma
  semi
  e <- optional parseExp
  semi
  s <- optional parseStmt
  rparen
  let types = map extractType ds
      extractType (Decl t _ _) = t
      extractType _ = error "Expected variable declaration in for loop"
  For types e s <$> parseBlock

parseBlock :: Parser [Stmt]
parseBlock = Debug.dbg "parseBlock" $ between lbrace rbrace (many parseStmt)

-- Declarations
parseTopDecl :: Parser Decl
parseTopDecl = Debug.dbg "parseTopDecl" parseFdecl

parseGdecl :: Parser Decl
parseGdecl = Debug.dbg "parseGdecl" $ do
  rword "global"
  ident <- identifier
  symbol "="
  e <- parseExp
  semi
  return $ Gdecl ident (Just e)

parseFdecl :: Parser Decl
parseFdecl = Debug.dbg "parseFdecl" $ do
  rt <- parseRetType
  ident <- identifier
  lparen
  args <- sepBy parseArg comma
  rparen
  Fdecl rt ident args <$> parseBlock

parseRetType :: Parser RetType
parseRetType =
  choice
    [ try $ RetVoid <$ rword "void",
      RetVal <$> parseType
    ]

parseArg :: Parser (OatType, Id)
parseArg = do
  t <- parseType
  ident <- identifier
  return (t, ident)

-- Program
parseProg :: Parser Prog
parseProg = Debug.dbg "parseProg" $ skipSpace *> many parseTopDecl <* eof
