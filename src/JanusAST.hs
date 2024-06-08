{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module JanusAST where
import Data.Map(Map)

data Value =
        NoneVal
    |   TrueVal | FalseVal
    |   IntVal Integer
    |   StringVal String
    |   ArrayVal Integer [Integer]
    deriving (Eq, Show, Read)


data Exp =
        EConst Value
    |   EVar VarVal
    |   EOp BinOp Exp Exp
    deriving (Eq, Show, Read)

data BinOp = Plus | Minus | Times | Div | Mod | Eq | Less | Greater | LOr | LAnd | XOr 
    deriving(Eq, Show, Read)


type VName = String
type PName = String

data Procedure = 
        Procedure PName [(VName, VarType)] Stmt
    |   Main [VarDecl] Stmt
    deriving(Eq, Show, Read)

type Program = [Procedure]

data VarDecl = IntV VName | AVar1 VName Exp | AVar2 VName Exp [Integer]
    deriving(Eq, Show, Read)

data VarVal = IVar VName | AVar VName Exp
    deriving(Eq, Show, Read)

data VarType = IntVar | ArrayVar
    deriving(Eq, Show, Read)

data Stmt = 
        SPluseq             VarVal Exp
    |   SMinuseq            VarVal Exp
    |   SXoreq              VarVal Exp
    |   SSwap               VarVal VarVal
    |   SSeq                Stmt Stmt
    |   SIfThenElse         Exp Stmt Stmt Exp --if exp1, then stmt1 else stmt2 fi exp2
    |   SFromDoLoopUntil    Exp Stmt Stmt Exp --from exp1 do stmt1 loop stmt2 until exp2 
    |   SCall               PName
    |   SUncall             PName
    |   SVarDecl            VName     
    deriving(Eq, Show, Read)  
