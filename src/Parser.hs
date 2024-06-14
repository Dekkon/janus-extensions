{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use unless" #-}
module Parser (parseProgram) where

import AST

import Control.Applicative ((<|>))
import Text.ParserCombinators.ReadP
import Data.Char
type Parser a = ReadP a
type ParseError = String

-- 
-- Grammar:  (found at https://tetsuo.jp/ref/janus.pdf, here with some modifications)
-- Program ::= { Procedure } MainProcedure { Procedure }
-- 
-- MainProcedure ::=  "Procedure" "main()" VarDecl Statement
-- 
-- Procedure ::= "procedure" ident "(" {TypeDecl} ")"  Statement
-- 
-- Stmt ::=    "If" Expr "Then" Stmt "Else" Stmt "Fi" Expr
--          |   "From" Expr "Do" Stmt "Loop" Stmt "Until" Expr
--          |   "Call" ident
--          |   "Uncall" ident
--          |   "+=" Expr | "-=" Expr | "^= Expr" 
--          |   Stmt Stmt
--          |   eps
-- 
-- Expr ::=    Expr2 { LogOp Expr2 }
-- 
-- Expr2 ::=    Expr3 [ RelOp Expr3 ]
-- 
-- Expr3 ::=    Expr4 { BitWOp Expr4 }
-- 
-- Expr4 ::=    Expr5 { AddOp Expr5}
-- 
-- Expr5 ::=    Expr6 { MulOp Expr6 }
-- 
-- Expr6 ::=    numConst
--          |   ident
--          |   ident "[" Expr "]"
--          |   "True" | "False"
--          |   "[" Expr { "," Expr } "]"
--          |   "(" Expr ")"
-- 
-- LogOp ::= "||" | "&&"
-- RelOp ::= "=" | "<" | ">"
-- BitWOp ::= "|" | "&" | "^"
-- AddOp ::= "+" | "-"
-- MulOp ::= "*" | "/" | "%"
--
--
-- VarDecl ::=  "int" ident | "int" ident"[" numConst "]" 
--            | "int" ident "[" numConst=i "]" "=" "{" numConst {"," numConst}*i-1 "}"
--                                                #this means whe must have the same amount of values here
--                                                #as is the size of the array. 
--   
-- TypeDecl ::= "int" ident | "int" indent "[]"  
--   
--   

-- helper functions from AP slides
whitespace :: Parser ()
whitespace = skipSpaces
lexeme :: Parser a -> Parser a
lexeme p = do a <- p; whitespace; parseComment; return a
symbol :: String -> Parser ()
symbol s = lexeme $ do string s; return ()
keyword :: String -> Parser ()
-- use munch here to force it to read the entire word
-- so that when checking against the given string the entire word 
-- needed to be read.
keyword s = lexeme $ do s' <- munch1 isAlphaNum
                        if s' == s then return ()
                        else pfail

reserved :: [String]
reserved =  [
              "procedure", "main", "if", "else", "fi", "from", "do", "loop",
              "until", "call", "uncall", "true", "false"
            ]

pIdent :: Parser String
pIdent = lexeme $ do
    c1 <- satisfy isAlpha
    -- using munch here as we want to just read the entire variable name
    -- don't want place where 3 ain, would be read as 
    -- (Op in (Const (IntVal 3), var "a")) and not an error
    crest <- munch (\c -> isAlphaNum c || c == '_')
    let iname = c1 : crest
    if iname `notElem` reserved then return iname
    else pfail


parseComment :: Parser ()
-- just calling whitespace after reaching newline will remove the newline
parseComment = do many (do string "//"; munch (/= '\n'); whitespace); return ()

parseProgram :: String -> Either ParseError Program
parseProgram s =
        case readP_to_S pProgram s of
          [] -> Left "Parse failed"
          [(p, "")] -> Right p
          e -> error $ show e
-- Program ::= { Procedure } MainProcedure { Procedure }
pProgram :: Parser Program
pProgram = do   whitespace; parseComment
                p1s <- many pProcedure
                mp <- pMainProcedure
                p2s <- many pProcedure
                eof
                return $ p1s ++ [("main", mp)] ++ p2s

-- MainProcedure ::=  "Procedure" "main()" VarDecl Statement
pMainProcedure :: Parser Procedure
pMainProcedure = do keyword "procedure"; keyword "main"; symbol "("; symbol ")";
                    vars <- pVarDecls; Main vars <$> pStmt;

-- Procedure ::= "procedure" ident "(" {TypeDecl} ")"  Statement
pProcedure :: Parser (PName, Procedure)
pProcedure = do   keyword "procedure"
                  pn <- pIdent
                  symbol "("
                  tdcls <- sepBy pTypeDecl (do symbol ",")
                  symbol ")"
                  s <- pStmt
                  return (pn, Procedure tdcls s)

--          |   Stmt Stmt
pStmt :: Parser Stmt
pStmt = do  s1 <- pStm
            option s1 $ do SSeq s1 <$> pStmt;


-- Stmt ::=    "If" Expr "Then" Stmt "Else" Stmt "Fi" Expr
--          |   "From" Expr "Do" Stmt "Loop" Stmt "Until" Expr
--          |   "Call" ident
--          |   "Uncall" ident
--          |   ident "+=" Expr | ident "-=" Expr | ident "^= Expr" 
--          |   Stmt Stmt
--          |   eps
pStm :: Parser Stmt
pStm =  do  v <- pVarVal -- do these whilst only parsing first vv one time
            do  symbol "+="; SPluseq v <$> pExp;
                <|> do  symbol "-="; SMinuseq v <$> pExp;
                <|> do  symbol "^="; SXoreq v <$> pExp;
                <|> do  symbol "<=>"; SSwap v <$> pVarVal;
        <|> do  keyword "call"; pn <- pIdent; SCall pn <$> pCall
        <|> do  keyword "uncall"; pn <- pIdent; SUncall pn <$> pCall
        <|> do  keyword "if"; e1 <- pExp; keyword "then"
                s1 <- pStmt; keyword "else"; s2 <- pStmt
                keyword "fi"; SIfThenElse e1 s1 s2 <$> pExp
        <|> do  keyword "from"; e1 <- pExp; keyword "do"
                s1 <- pStmt; keyword "loop"; s2 <- pStmt
                keyword "until"; e2 <- pExp; return $ SFromDoLoopUntil e1 s1 s2 e2 True
        <|> do  keyword "map"; cou <- pCallorUncall; pn <- pIdent
                SCompinator Map cou pn <$> pIdent

pCallorUncall :: Parser CallOrUncall
pCallorUncall = do keyword "uncall"; return Uncall
            <|> do keyword "call"; return Call 


--this parser assumes that 
--"call" or "uncall" has just been parsed
pCall :: Parser [ArgVar]
pCall = do  symbol "(";
            varlist <- sepBy 
                (do n <- pIdent; 
                    option (NVar n) $
                        do  symbol "["
                            e <- pExp
                            symbol "]"
                            return $ IndexVar n e
                ) 
                (symbol ",")
            symbol ")"
            return varlist

-- pStm :: Parser Stmt
-- pStm =  do v <- pVarVal; symbol "+="; SPluseq v <$> pExp;
--     <|> do v <- pVarVal; symbol "-=";  v <$> pExp;

-- VarDecls ::= { VarDecl }
pVarDecls :: Parser [VarDecl]
pVarDecls = many pVarDecl

-- VarDecl ::=  "int" ident | "int" ident"[" numConst "]" 
--            | "int" ident "[" numConst=i "]" "=" "{" numConst {"," numConst}*i-1 "}"
--                                                #this means whe must have the same amount of values here
--                                                #as is the size of the array. 
pVarDecl :: Parser VarDecl
pVarDecl = do keyword "int"; pVarDeclVar


-- TypeDecl ::= "int" ident | "int" indent "[]" 
pTypeDecl :: Parser (VName, VarType)
pTypeDecl = do  keyword "int"; vn <- pIdent
                option (vn, IntVar) $
                    do symbol "[]"; return (vn, ArrayVar)


-- Expr ::=    Expr2 { LogOp Expr2 }
pExp :: Parser Exp
pExp = pExpr2 `chainl1` pLogOp

-- Expr2 ::=    Expr3 [ RelOp Expr3 ]
pExpr2 :: Parser Exp
pExpr2 = do e1 <- pExpr3
            option e1 $ do relOp <- pRelOp; relOp e1 <$> pExpr3;


-- Expr2 ::=    Expr3 { BitWOp Expr3 }
pExpr3 :: Parser Exp
pExpr3 = pExpr4 `chainl1` pBitWOp


-- Expr3 ::=    Expr4 { AddOp Expr4}
pExpr4 :: Parser Exp
pExpr4 = pExpr5 `chainl1` pAddOp


-- Expr4 ::=    Expr5 { MulOp Expr5 }
pExpr5 :: Parser Exp
pExpr5 = pExpr6 `chainl1` pMulOp


-- Expr5 ::=    numConst
--          |   ident
--          |   ident "[" Expr "]"
--          |   "true" | "false"
--          |   "(" Expr ")"
pExpr6 :: Parser Exp
pExpr6 =    do  name <- pIdent
                option (EVar name) $
                    do  symbol "["
                        num <- pExp -- num is the index or length of array
                        symbol "]"
                        return $ EArrIndex name num
        <|> pNumConst
        <|> do keyword "true"; return $ EConst $ BoolVal True
        <|> do keyword "false"; return $ EConst $ BoolVal False
        <|> do symbol "("; e <- pExp; symbol ")"; return e


-- RelOp ::= "=" | "<" | ">"
pRelOp :: Parser (Exp -> Exp -> Exp)
pRelOp =    do symbol "="; return $ EOp Eq
        <|> do symbol "<"; return $ EOp Less
        <|> do symbol ">"; return $ EOp Greater
-- BitWOp ::= "|" | "&" | "^"
pBitWOp :: Parser (Exp -> Exp -> Exp)
pBitWOp =   do symbol "&"; return $ EOp BAnd
        <|> do symbol "|"; return $ EOp BOr
        <|> do symbol "^"; return $ EOp XOr
-- LogOp ::= "||" | "&&"
pLogOp :: Parser (Exp -> Exp -> Exp)
pLogOp =    do symbol "&&"; return $ EOp LAnd
        <|> do symbol "||"; return $ EOp LOr
-- AddOp ::= "+" | "-"
pAddOp :: Parser (Exp -> Exp -> Exp)
pAddOp =    do symbol "+"; return $ EOp Plus
        <|> do symbol "-"; return $ EOp Minus
-- MulOp ::= "*" | "/" | "%"
pMulOp :: Parser (Exp -> Exp -> Exp)
pMulOp =    do symbol "*"; return $ EOp Times
        <|> do symbol "/"; return $ EOp Div

pNumConst :: Parser Exp
pNumConst =
    do isPos <- pN2
       num <- pN3
       return $ EConst $ IntVal $ num * if isPos then 1 else -1
pN2 :: Parser Bool
pN2 = do string "-"; return False
      <++ return True
pN3 :: Parser Integer
pN3 = do symbol "0"; return 0
  <++ do lexeme $ do
            d1 <- satisfy (`elem` ['1'..'9'])
            -- using munch as we would always want all the digits in a number
            drest <- munch isDigit
            return $ read $ d1 : drest

pJustNum :: Parser Integer
pJustNum = do (EConst (IntVal n)) <- pNumConst; return n


pVarVal :: Parser VarVal
pVarVal = do    name <- pIdent
                option (IVar name) $
                    do  symbol "["
                        num <- pExp -- num is the index or length of array
                        symbol "]"
                        return $ AVar name num
pVarDeclVar :: Parser VarDecl
pVarDeclVar = do   name <- pIdent
                   option (IntV name) $
                        do  symbol "["
                            num <- pJustNum -- num is the index or length of array
                            symbol "]"
                            option (AVar1 name num) $
                                do  symbol "="; symbol "{";
                                    l <- sepBy1 pJustNum (symbol ",")
                                    symbol "}"
                                    return $ AVar2 name num l
