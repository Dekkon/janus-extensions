{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}


module Interpreter where


import AST
import Control.Monad
import qualified Data.Map.Strict as M
import Data.Map(Map)
import Data.Bits


type Env = Map VName Value
type PEnv = Map PName Procedure

data Error =    BadVar String | BadProc String | BadIndex String | BadVal String
            |   BadAssertion String | BadArity String
    deriving(Eq, Show, Read)

newtype Comp a = Comp {runComp :: PEnv -> Env -> Either Error (a, Env)}

instance Monad Comp where
  return a = Comp (\_ env -> return (a, env) )
  (>>=) m f = Comp (\fenv env ->
            do  (a, env2) <- runComp m fenv env
                runComp (f a) fenv env2
        )


-- needed to be able to define monad
instance Functor Comp where
  fmap = liftM
instance Applicative Comp where
  pure = return; (<*>) = ap

getVar :: VName -> Comp Value
getVar vn = Comp (\_ env ->
                    case M.lookup vn env of
                        Nothing -> Left $ BadVar ("Variable " ++ vn ++ " has not been declared")
                        Just value -> return (value, env)
                )

setVar :: VName -> Value -> Comp ()         --insert in env, overwrite if already there
setVar vn val = Comp (\_ env -> return ((), M.insert vn val env))

getEnv :: Comp Env
getEnv = Comp (\_ env -> return (env, env))

getProc :: PName -> Comp Procedure
getProc pn = Comp   (\penv env ->
                        case M.lookup pn penv of
                            Nothing -> Left $ BadProc ("Procedure " ++ pn ++ " has not been declared")
                            Just p -> return (p, env)
                    )

withEnv :: Env -> Comp a -> Comp Env
withEnv env2 m = Comp (\penv env -> do
                            (_, env3) <- runComp m penv env2
                            return (env3, env)
                            -- returns the new env and restores the old
                      )

raiseError :: Error -> Comp a
raiseError err = Comp (\_ _ -> Left err)

-- returns what the index was and the number found there
indexArrayAtExp :: VName -> Exp -> Comp (Integer, Integer)
indexArrayAtExp a_name e_indx = do
    arr <- getVar a_name
    case arr of
        ArrayVal size arr_l -> do
            res <- evalExp e_indx
            case res of
                IntVal i ->
                    if i < 0 || i >= size then
                        raiseError $ BadIndex ("Array index: " ++ show i ++ " is out of bounds")
                    else return (i, arr_l !! fromIntegral i)
                _ -> raiseError $ BadIndex "Array index must be a number"
        _ -> raiseError $ BadVar ("Variable " ++ a_name ++ " is not an array")


-- we assume this is only ever called after indexarray at exp
-- thus we need not to check if index out of bounds.
setValInArrayAtIndex :: VName -> Integer -> Integer -> Comp ()
setValInArrayAtIndex a_name i newnum = do
    arr <- getVar a_name
    case arr of
        ArrayVal size arr_l -> do
            let new_arr = (let (ls, rs) = splitAt (fromIntegral i) arr_l in ls ++ [newnum] ++ tail rs) in
                setVar a_name (ArrayVal size new_arr)
        _ -> raiseError $ BadVar ("Variable " ++ a_name ++ " is not an array")

getIntVal :: Value -> Error -> Comp Integer
getIntVal (IntVal n) _  = return n
getIntVal _ e  = raiseError e


getIntVar :: VName -> Comp Integer
getIntVar vn = do
    val <- getVar vn
    getIntVal val (BadVar ("Variable: " ++ vn ++ " must be an integer"))


getArrayVar :: VName -> Comp (Integer, [Integer])
getArrayVar vn = do
    val <- getVar vn
    getArrayVal val (BadVar ("Variable: " ++ vn ++ " must be an "))

getArrayVal :: Value -> Error -> Comp (Integer, [Integer])
getArrayVal (ArrayVal len arr) _  = return (len, arr)
getArrayVal _ e  = raiseError e

getBoolVal :: Value -> Error -> Comp Bool
getBoolVal (BoolVal b) _  = return b
getBoolVal _ e  = raiseError e

getBoolValExpErr :: Value -> Comp Bool
getBoolValExpErr v = getBoolVal v (BadVal "Expression must evaluate to boolean")

operate :: BinOp -> Value -> Value -> Either String Value
operate Plus (IntVal v1) (IntVal v2) = return $ IntVal (v1 + v2)
operate Minus (IntVal v1) (IntVal v2) = return $ IntVal (v1 - v2)
operate Times (IntVal v1) (IntVal v2) = return $ IntVal (v1 * v2)
operate Div (IntVal v1) (IntVal v2)
    | v2 == 0 = Left "Divide by zero"
    | otherwise = return $ IntVal (v1 `div` v2)
operate Mod (IntVal v1) (IntVal v2)
    | v2 == 0 = Left "Divide by zero"
    | otherwise = return $ IntVal (v1 `mod` v2)
operate BAnd (IntVal v1) (IntVal v2) = return $ IntVal (v1 .&. v2)
operate BOr (IntVal v1) (IntVal v2) = return $ IntVal (v1 .|. v2)
operate XOr (IntVal v1) (IntVal v2) = return $ IntVal (v1 `xor` v2)
operate Eq (IntVal v1) (IntVal v2) = return $ BoolVal (v1 == v2)
operate Less (IntVal v1) (IntVal v2) = return $ BoolVal (v1 < v2)
operate Greater (IntVal v1) (IntVal v2) = return $ BoolVal (v1 < v2)
operate LAnd (BoolVal v1) (BoolVal v2) = return $ BoolVal (v1 && v2)
operate LOr (BoolVal v1) (BoolVal v2) = return $ BoolVal (v1 || v2)
operate op v1 v2 = Left $ "Cannot perform " ++ show op ++ " on the values "
                            ++ show v1 ++ " and " ++ show v2


evalExp :: Exp -> Comp Value
evalExp (EConst val) = return val
evalExp (EVar vn) = getVar vn
evalExp (EArrIndex vn e) = do
    (_, n) <- indexArrayAtExp vn e; return $ IntVal n
evalExp (EOp op e1 e2) = do
    v1 <- evalExp e1
    v2 <- evalExp e2
    case operate op v1 v2 of
        Left e -> raiseError $ BadVal e
        Right v -> return v

-- updateStatement :: Stmt -> Comp ()
updateStatement :: (Integer -> Integer -> Integer) -> VarVal -> Exp -> Comp ()
updateStatement op vv e = do
    val2 <-  evalExp e
    n2 <- getIntVal val2 (BadVal "Expression must evaluate to an integer")
    case vv of
        IVar vn -> do
            orig_val <- getVar vn
            n1 <- getIntVar vn
            setVar vn (IntVal $ op n1 n2)
        AVar vn e_indx -> do
            (i, n1) <- indexArrayAtExp vn e_indx
            setValInArrayAtIndex vn i (op n1 n2)


executeStmt :: Stmt -> Comp ()
executeStmt (SSeq s1 s2) = executeStmt s1 >> executeStmt s2
executeStmt (SPluseq vv e) = updateStatement (+) vv e
executeStmt (SMinuseq vv e) = updateStatement (-) vv e
executeStmt (SXoreq vv e) = updateStatement xor vv e
-- the swab one can maybe be done in a nicer way.
executeStmt (SSwap (IVar v1) (IVar v2)) = do
    n1 <- getIntVar v1; n2 <- getIntVar v2
    setVar v1 (IntVal n2); setVar v2 (IntVal n1)
executeStmt (SSwap (IVar v1) (AVar v2 e2)) = do
    n1 <- getIntVar v1; (i2, n2) <- indexArrayAtExp v2 e2
    setVar v1 (IntVal n2); setValInArrayAtIndex v2 i2 n1
executeStmt (SSwap (AVar v1 e1) (IVar v2)) = do
    (i1, n1) <- indexArrayAtExp v1 e1; n2 <- getIntVar v2;
    setValInArrayAtIndex v1 i1 n2; setVar v2 (IntVal n1)
executeStmt (SSwap (AVar v1 e1) (AVar v2 e2)) = do
    (i1, n1) <- indexArrayAtExp v1 e1; (i2, n2) <- indexArrayAtExp v2 e2
    setValInArrayAtIndex v1 i1 n2; setValInArrayAtIndex v2 i2 n1
executeStmt (SIfThenElse e1 s1 s2 e2) = do
    val1 <- evalExp e1
    b1 <- getBoolValExpErr val1
    if b1 then do
        executeStmt s1
        val2 <- evalExp e2
        b2 <- getBoolValExpErr val2
        if b2 then return ()
        else raiseError $ BadAssertion "Assertion should be true"
    else do
        executeStmt s2
        val2 <- evalExp e2
        b2 <- getBoolValExpErr val2
        if not b2 then return ()
        else raiseError $ BadAssertion "Assertion should be false"
-- if firstIter is true, then initial condition must be true
-- if it is false, then initial condition must be false
executeStmt (SFromDoLoopUntil e1 s1 s2 e2 firstIter) = do
    val1 <- evalExp e1
    b1 <- getBoolValExpErr val1
    if b1 == firstIter then do
        executeStmt s1
        val2 <- evalExp e2
        b2 <- getBoolValExpErr val2
        if b2 then return ()
        --enter loop again, where we are not in firstiter
        else do
            executeStmt s2 
            executeStmt (SFromDoLoopUntil e1 s2 s2 e2 False)
    else raiseError $ BadAssertion ("Assertion should be " ++ show firstIter)
executeStmt (SCall pn input_varlist) = callOrUncallProc pn input_varlist executeStmt
executeStmt (SUncall pn input_varlist) = callOrUncallProc pn input_varlist unexecuteStmt

callOrUncallProc :: PName -> [VName] -> (Stmt -> Comp ()) -> Comp ()
callOrUncallProc pn input_varlist stmt_execution = do 
    p <- getProc pn
    case p of
        Main _ _ -> raiseError $ BadVar "this should be impossible"
        Procedure proc_varlist s -> do
            p_env <- createNewEnv input_varlist proc_varlist
                                    --whether this is exe or unexe
                                    --is based on if call/uncall called this
            new_env <- withEnv p_env (stmt_execution s)
            updateOldEnv input_varlist proc_varlist new_env

unexecuteStmt :: Stmt -> Comp ()
unexecuteStmt (SSeq s1 s2) = unexecuteStmt s2 >> unexecuteStmt s1
unexecuteStmt (SPluseq vv e) = updateStatement (-) vv e
unexecuteStmt (SMinuseq vv e) = updateStatement (+) vv e
unexecuteStmt (SXoreq vv e) = updateStatement xor vv e --xor is its own inverse
unexecuteStmt s@(SSwap _ _) = executeStmt s -- swap is its own inverse so just call original swap
unexecuteStmt (SIfThenElse e1 s1 s2 e2) = do
    val2 <- evalExp e2
    b2 <- getBoolValExpErr val2
    if b2 then do
        unexecuteStmt s1
        val1 <- evalExp e1
        b1 <- getBoolValExpErr val1
        if b1 then return ()
        else raiseError $ BadAssertion "Assertion should be true"
    else do
        unexecuteStmt s2
        val1 <- evalExp e1
        b1 <- getBoolValExpErr val1
        if not b1 then return ()
        else raiseError $ BadAssertion "Assertion should be false"
unexecuteStmt (SFromDoLoopUntil e1 s1 s2 e2 firstIter) = do
    val2 <- evalExp e2
    b2 <- getBoolValExpErr val2
    if b2 == firstIter then do
        unexecuteStmt s1
        val1 <- evalExp e1
        b1 <- getBoolValExpErr val1
        if b1 then return ()
        --enter loop again, where we are not in firstiter
        else do
            unexecuteStmt s2 
            unexecuteStmt (SFromDoLoopUntil e1 s2 s2 e2 False)
    else raiseError $ BadAssertion ("Assertion should be " ++ show firstIter)
            -- when calling normally within undo we wanna contiue undoing
unexecuteStmt (SCall pn input_varlist) = callOrUncallProc pn input_varlist unexecuteStmt
            -- when uncalling within undo, we wanna just do
unexecuteStmt (SUncall pn input_varlist) = callOrUncallProc pn input_varlist executeStmt

createNewEnv :: [VName] -> [(VName, VarType)] -> Comp Env
createNewEnv in_vlist p_vlist = do
    if length in_vlist /= length p_vlist then
        raiseError $ BadArity "arity of procedure doesn't match argument arity"
    else createEnvHelper in_vlist p_vlist
    where
        createEnvHelper :: [VName] -> [(VName, VarType)] -> Comp Env
        createEnvHelper [] [] = return M.empty
        createEnvHelper (v1 : l1s) ((v2, v2t) : l2s) =  do
            case v2t of
                IntVar -> do
                        -- this will fail if v1 is not an integer
                    n <- getIntVar v1
                    env2 <- createEnvHelper l1s l2s
                    return $ M.insert v2 (IntVal n) env2
                ArrayVar -> do
                                -- this will fail if v1 is not an array
                    (len, arr) <- getArrayVar v1
                    env2 <- createEnvHelper l1s l2s
                    return $ M.insert v2 (ArrayVal len arr) env2

updateOldEnv :: [VName] -> [(VName, VarType)] -> Env -> Comp ()
updateOldEnv [] [] _ = return ()
updateOldEnv (v1 : l1s) ((v2, _) : l2s) n_env =
    case M.lookup v2 n_env of
        Nothing -> raiseError $ BadVar "should not happen"
        Just val -> setVar v1 val >> updateOldEnv l1s l2s n_env

setUpEnv :: Procedure -> Env
setUpEnv (Main vds _) = setUpEnvHelper vds
    where 
        setUpEnvHelper [] = M.empty
        setUpEnvHelper (vd : vs) = 
            let (var_name, val) =
                    case vd of
                        IntV vn -> (vn, IntVal 0)
                        AVar1 vn len -> if len < 1 then error "oof" else (vn, ArrayVal len (replicate (fromIntegral len) 0))
                        AVar2 vn len arr -> if len < 1 || length arr /= fromIntegral len then error "oof" else (vn, ArrayVal len arr)
            in M.insert var_name val (setUpEnvHelper vs)
setUpEnv _ = error "only call setup env with main"

executeProgram :: Program -> Either Error ((), Env)
executeProgram proc_list =
    let penv = M.fromList proc_list
        main = case M.lookup "main" penv of
                Just m -> m
                Nothing -> error "no main function, program shouldn't have been able to parse"
        penv2 = M.delete "main" penv --don't want main to be callable
        env = setUpEnv main
        main_s = case main of
                    Main _ s -> s
                    _ -> error "not main"
    in runComp (executeStmt main_s) penv2 env
