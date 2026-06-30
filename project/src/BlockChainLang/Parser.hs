{-# LANGUAGE OverloadedStrings #-}

-- | Lexer and parser for BlockChainLang, built on megaparsec over 'Text'.
module BlockChainLang.Parser
  ( Parser
  , parseContract
  , parseExpr
  , pContract
  , pStatement
  , pExpr
  , pType
  ) where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import BlockChainLang.Syntax

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens, braces, brackets, angles :: Parser a -> Parser a
parens   = between (symbol "(") (symbol ")")
braces   = between (symbol "{") (symbol "}")
brackets = between (symbol "[") (symbol "]")
angles   = between (symbol "<") (symbol ">")

keywords :: [Text]
keywords =
  [ "contract", "state", "transaction", "require", "if", "else"
  , "empty", "sender", "true", "false", "not"
  , "int", "bool", "address", "map"
  ]

identChar :: Parser Char
identChar = alphaNumChar <|> char '_'

reserved :: Text -> Parser ()
reserved kw = (lexeme . try) (string kw *> notFollowedBy identChar)

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> (letterChar <|> char '_') <*> many identChar
    check x
      | T.pack x `elem` keywords =
          fail $ "keyword " ++ show x ++ " cannot be an identifier"
      | otherwise = pure x

integer :: Parser Integer
integer = lexeme L.decimal

-- | Address literal, written @\@name@.
addrLit :: Parser Text
addrLit = lexeme (char '@' *> (T.pack <$> some identChar))

pExpr :: Parser Expr
pExpr = makeExprParser pTerm opTable

pTerm :: Parser Expr
pTerm = foldl Index <$> pPrimary <*> many (brackets pExpr)

pPrimary :: Parser Expr
pPrimary =
      parens pExpr
  <|> Sender            <$  reserved "sender"
  <|> Empty             <$  reserved "empty"
  <|> Lit (LBool True)  <$  reserved "true"
  <|> Lit (LBool False) <$  reserved "false"
  <|> Lit . LInt        <$> integer
  <|> Lit . LAddr       <$> addrLit
  <|> Var               <$> identifier

opTable :: [[Operator Parser Expr]]
opTable =
  [ [ Prefix (UnOp Not <$ reserved "not") ]
  , [ binL "*" Mul ]
  , [ binL "+" Add, binL "-" Sub ]
  , [ binN "==" Eq, binN "/=" Neq
    , binN "<=" Le, binN ">=" Ge
    , binN "<"  Lt, binN ">"  Gt
    ]
  , [ binL "&&" And ]
  , [ binL "||" Or ]
  ]
  where
    binL name op = InfixL (BinOp op <$ symbol name)
    binN name op = InfixN (BinOp op <$ symbol name)

pType :: Parser Type
pType =
      TInt     <$ reserved "int"
  <|> TBool    <$ reserved "bool"
  <|> TAddress <$ reserved "address"
  <|> pMap
  where
    pMap = do
      reserved "map"
      angles (TMap <$> pType <* symbol "," <*> pType)

pStatement :: Parser Statement
pStatement = pRequire <|> pIf <|> pAssign
  where
    pRequire = Require <$> (reserved "require" *> pExpr <* symbol ";")

    pIf = do
      reserved "if"
      cond <- parens pExpr
      thn  <- braces (many pStatement)
      els  <- option [] (reserved "else" *> braces (many pStatement))
      pure (If cond thn els)

    pAssign = do
      lhs <- pExpr
      _   <- symbol ":="
      rhs <- pExpr
      _   <- symbol ";"
      pure (Assign lhs rhs)

pStateVar :: Parser StateVar
pStateVar = do
  name <- identifier
  _    <- symbol ":"
  ty   <- pType
  _    <- symbol "="
  ini  <- pExpr
  _    <- symbol ";"
  pure (StateVar name ty ini)

pParam :: Parser (String, Type)
pParam = (,) <$> identifier <* symbol ":" <*> pType

pTransaction :: Parser TransactionDef
pTransaction = do
  reserved "transaction"
  name <- identifier
  ps   <- parens (pParam `sepBy` symbol ",")
  body <- braces (many pStatement)
  pure (TransactionDef name ps body)

pContract :: Parser Contract
pContract = do
  reserved "contract"
  _name <- identifier
  braces $ do
    st  <- reserved "state" *> braces (many pStateVar)
    txs <- many pTransaction
    pure (Contract st txs)

parseContract :: Text -> Either (ParseErrorBundle Text Void) Contract
parseContract = runParser (sc *> pContract <* eof) "<input>"

parseExpr :: Text -> Either (ParseErrorBundle Text Void) Expr
parseExpr = runParser (sc *> pExpr <* eof) "<expr>"
