{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module AST where

import Data.Map(Map)

data Value =
        NoneVal
    |   BoolVal Bool
    |   IntVal Integer
    |   StringVal String
    |   ArrayVal Integer [Integer]
    deriving (Eq, Show, Read)


data Exp =
        EConst Value
    |   EVar VName
    |   EArrIndex VName Exp
    |   EOp BinOp Exp Exp
    deriving (Eq, Show, Read)

data BinOp = Plus | Minus | Times | Div | Mod | Eq | Less | Greater | LOr | LAnd | BOr | BAnd | XOr 
    deriving(Eq, Show, Read)


type VName = String
type PName = String

data Procedure = 
        Procedure [(VName, VarType)] Stmt
    |   Main [VarDecl] Stmt
    deriving(Eq, Show, Read)

type Program = [(PName, Procedure)]

data VarDecl = IntV VName | AVar1 VName Integer | AVar2 VName Integer [Integer]
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
    |   SCall               PName [VName] 
    |   SUncall             PName [VName]
    |   SIfThenElse         Exp Stmt Stmt Exp --if exp1, then stmt1 else stmt2 fi exp2
                -- bool here to indicate whether in first or subsequent iterations of loop
    |   SFromDoLoopUntil    Exp Stmt Stmt Exp Bool --from exp1 do stmt1 loop stmt2 until exp2     
    deriving(Eq, Show, Read)  
