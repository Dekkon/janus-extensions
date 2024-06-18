{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use zipWith" #-}


module Interpreter where


import AST
import Control.Monad
import qualified Data.Map.Strict as M
import Data.Map(Map)
import Data.Bits
import Data.List (nub)


type Env = Map VName Value
type PEnv = Map PName Procedure

type Error = (ErrorType, String)
data ErrorType =    BadVar String | BadProc String | BadIndex String | BadVal String
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
                        Nothing -> Left (BadVar ("Variable " ++ vn ++ " has not been declared"), show env)
                        Just value -> return (value, env)
                )

setVar :: VName -> Value -> Comp ()         --insert in env, overwrite if already there
setVar vn val = Comp (\_ env -> return ((), M.insert vn val env))

getEnv :: Comp Env
getEnv = Comp (\_ env -> return (env, env))

getProc :: PName -> Comp Procedure
getProc pn = Comp   (\penv env ->
                        case M.lookup pn penv of
                            Nothing -> Left $ (BadProc ("Procedure " ++ pn ++ " has not been declared"), show env)
                            Just p -> return (p, env)
                    )

withEnv :: Env -> Comp a -> Comp Env
withEnv env2 m = Comp (\penv env -> do
                            (_, env3) <- runComp m penv env2
                            return (env3, env)
                            -- returns the new env and restores the old
                      )

raiseError :: ErrorType -> Comp a
raiseError err = Comp (\_ env -> Left (err, show env))

getProcedure :: PName -> Comp ([(VName, VarType)], [VarDecl], Stmt)
getProcedure pn = do
    p <- getProc pn
    case p of
        Main _ _ -> raiseError $ BadVar "this should be impossible"
        Procedure proc_varlist vdcls s -> return (proc_varlist, vdcls, s)


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

getIntVal :: Value -> ErrorType -> Comp Integer
getIntVal (IntVal n) _  = return n
getIntVal _ e  = raiseError e


getIntVar :: VName -> Comp Integer
getIntVar vn = do
    val <- getVar vn
    getIntVal val (BadVar ("Variable: " ++ vn ++ " must be an integer"))


getArrayVar :: VName -> Comp (Integer, [Integer])
getArrayVar vn = do
    val <- getVar vn
    getArrayVal val (BadVar ("Variable: " ++ vn ++ " must be an array var"))

getArrayVal :: Value -> ErrorType -> Comp (Integer, [Integer])
getArrayVal (ArrayVal len arr) _  = return (len, arr)
getArrayVal _ e  = raiseError e

getBoolVal :: Value -> ErrorType -> Comp Bool
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
operate Greater (IntVal v1) (IntVal v2) = return $ BoolVal (v1 > v2)
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
        else raiseError $ BadAssertion $ "Assertion should be false " ++ show e2
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
executeStmt (SCompinator Map cou pn vn) = do
    (len, arr) <- getArrayVar vn
    (vlist, vds, s) <- getProcedure pn
    if length vlist /= 1 then raiseError $ BadArity "proc in map must only take 1 var"
    else
        case vlist of
            [(x, IntVar)] ->
                mapM_ (\i ->
                        executeStmt (createCallOrUncall cou pn [IndexVar vn (EConst $ IntVal i)]))
                    [0..len - 1]
            _ -> raiseError $ BadVar "proc in map must take int as its argument"
executeStmt (SCompinator Scanl cou pn vn) = do
    (len, arr) <- getArrayVar vn
    (vlist, vds, s) <- getProcedure pn
    if length vlist /= 2 then raiseError $ BadArity "proc in map must only take 2 vars"
    else
        case vlist of
            [(x1, IntVar), (x2, IntVar)] ->
                mapM_ (\(i, j) ->
                        executeStmt
                            (createCallOrUncall cou pn
                                [IndexVar vn (EConst $ IntVal i)
                                ,IndexVar vn (EConst $ IntVal j)]))
                    (zip [0..len - 2] [1..len - 1] )
            _ -> raiseError $ BadVar "proc in map must take two ints as its arguments"
executeStmt (SCompinator Scanlw cou pn vn) = do
    (len, arr) <- getArrayVar vn
    (vlist, vds, s) <- getProcedure pn
    mapM_ (\ arg_v ->  -- check that procedure only takes ints as args
        case arg_v of
            (_, IntVar) -> return ()
            _ -> raiseError $ BadVar "proc in scan must only take ints as arguments")
        vlist
    mapM_ (\ i ->
                executeStmt
                    (
                        createCallOrUncall cou pn (map (\ j -> IndexVar vn (EConst $ IntVal j)) [i..i-1+toInteger (length vlist)])
                    )
            )
        [0..len - toInteger (length vlist) ]
executeStmt (SCompinator Scanrw cou pn vn) = do
    (len, arr) <- getArrayVar vn
    (vlist, vds, _) <- getProcedure pn
    mapM_ (\ arg_v ->  -- check that procedure only takes ints as args
        case arg_v of
            (_, IntVar) -> return ()
            _ -> raiseError $ BadVar "proc in scan must only take ints as arguments")
        vlist
    mapM_ (\ i ->
                executeStmt
                    (
                        createCallOrUncall cou pn (map (\ j -> IndexVar vn (EConst $ IntVal j)) [i+1-toInteger (length vlist)..i])
                    )
            )
        (reverse [toInteger (length vlist)-1..len -1 ])
executeStmt (SScanlwz cou pn arr_vlist) = do
        (args_per_array, arr_len) <- setupScanwz cou pn arr_vlist
        mapM_ (\ i ->
                    executeStmt
                        (
                            let var_input = 
                                        concatMap (\ an ->
                                                    map (\ j -> IndexVar an (EConst $ IntVal j)) [i..i-1+ args_per_array  ]
                                                  ) arr_vlist
                            in
                                createCallOrUncall cou pn var_input
                        )
                )
            [0..arr_len - args_per_array ]
executeStmt (SScanrwz cou pn arr_vlist) = do
        (args_per_array, arr_len) <- setupScanwz cou pn arr_vlist
        mapM_ (\ i ->
                    executeStmt
                        (
                            let var_input = 
                                        concatMap (\ an ->
                                                    map (\ j -> IndexVar an (EConst $ IntVal j)) [i+ 1 - args_per_array..i ]
                                                  ) arr_vlist
                            in
                                createCallOrUncall cou pn var_input
                        )
                )
            (reverse [args_per_array-1..arr_len - 1 ])
executeStmt (SIota vn) = do
    (len, arr) <- getArrayVar vn
    mapM_ (\i -> setValInArrayAtIndex vn i (i + arr !! fromIntegral i) ) [0 .. len-1]
executeStmt (SAtoi vn) = do
    (len, arr) <- getArrayVar vn
    mapM_ (\i -> setValInArrayAtIndex vn i (arr !! fromIntegral i - i) ) [0 .. len-1]


setupScanwz :: CallOrUncall -> PName -> [VName] -> Comp (Integer, Integer)
setupScanwz cou pn arr_vlist = do 
    len_arr_lists <- mapM getArrayVar arr_vlist
    (vlist, vds, _) <- getProcedure pn
    let (lenghts_list, arr_lists) = unzip len_arr_lists
        c_arr_length = case nub lenghts_list of
                        [l] -> return l
                        _ -> raiseError $ BadVar "all arrays in input must have same length"
        number_of_args = length vlist
        number_of_arrays = length arr_vlist
        args_per_array = toInteger $ number_of_args`div` number_of_arrays
        in do
            arr_len <- c_arr_length
            when (number_of_args `mod` number_of_arrays /= 0) $ raiseError $ BadArity "proc must have input amount divisible by number of arrays"
            mapM_ (\ arg_v ->  -- check that procedure only takes ints as args
                case arg_v of
                    (_, IntVar) -> return ()
                    _ -> raiseError $ BadVar "proc in scan must only take ints as arguments")
                vlist
            return (args_per_array, arr_len)



createCallOrUncall :: CallOrUncall -> PName -> [ArgVar] -> Stmt
createCallOrUncall Call = SCall
createCallOrUncall Uncall = SUncall

callOrUncallProc :: PName -> [ArgVar] -> (Stmt -> Comp ()) -> Comp ()
callOrUncallProc pn input_varlist stmt_execution = do
    (proc_varlist, vardecls, s) <- getProcedure pn
    old_env <- getEnv
    var_decl_env <- setUpEnvComp vardecls
    arg_env <- createNewEnv input_varlist proc_varlist
                                --whether this is exe or unexe
                                --is based on if call/uncall called this
    let proc_env = M.union var_decl_env arg_env
        in do
            when (M.size proc_env /= length input_varlist + length vardecls) $ raiseError $ BadProc "proc cannot have variables with the same name"
            new_env <- withEnv proc_env (stmt_execution s)
            mapM_   (\(vd_name, vd_val) -> 
                        case M.lookup vd_name new_env of
                            Nothing -> raiseError $ BadVar "this shouldn't be possible"
                            Just val -> when (vd_val /= val) $ raiseError $ BadVar "local variables in procedure must return to initial value"
                    ) (M.toList var_decl_env)
            updateOldEnv input_varlist proc_varlist old_env new_env

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
unexecuteStmt (SCompinator Map cou pn vn) =
    executeStmt (SCompinator Map (swapCallUnCall cou) pn vn)
    -- case cou of 
    --     Call -> executeStmt (SCompinator Map Uncall pn vn)
    --     Uncall -> executeStmt (SCompinator Map Call pn vn)
unexecuteStmt (SCompinator Scanl cou pn vn) = do
    (len, arr) <- getArrayVar vn
    (vlist, vds, s) <- getProcedure pn
    if length vlist /= 2 then raiseError $ BadArity "proc in map must take 2 vars"
    else
        case vlist of
            [(x1, IntVar), (x2, IntVar)] ->
                mapM_ (\(i, j) ->
                        executeStmt
                            (createCallOrUncall (swapCallUnCall cou) pn
                                [IndexVar vn (EConst $ IntVal (toInteger i))
                                ,IndexVar vn (EConst $ IntVal (toInteger j))]))
                    (zip (reverse [0..length arr - 2]) (reverse [1..length arr - 1]) )
            _ -> raiseError $ BadVar "proc in map must take two ints as its arguments"
unexecuteStmt (SCompinator Scanlw cou pn vn) =
    executeStmt (SCompinator Scanrw (swapCallUnCall cou) pn vn)
unexecuteStmt (SCompinator Scanrw cou pn vn) =
    executeStmt (SCompinator Scanlw (swapCallUnCall cou) pn vn)
unexecuteStmt (SScanlwz cou pn arr_vlist) = 
    executeStmt (SScanrwz (swapCallUnCall cou) pn arr_vlist)
unexecuteStmt (SScanrwz cou pn arr_vlist) = 
    executeStmt (SScanlwz (swapCallUnCall cou) pn arr_vlist)
unexecuteStmt (SIota v) = executeStmt (SAtoi v)
unexecuteStmt (SAtoi v) = executeStmt (SIota v)

swapCallUnCall :: CallOrUncall -> CallOrUncall
swapCallUnCall Call = Uncall
swapCallUnCall Uncall = Call



createNewEnv :: [ArgVar] -> [(VName, VarType)] -> Comp Env
createNewEnv in_vlist p_vlist = do
    if length in_vlist /= length p_vlist then
        raiseError $ BadArity "arity of procedure doesn't match argument arity"
    else createEnvHelper in_vlist p_vlist
    where
        createEnvHelper :: [ArgVar] -> [(VName, VarType)] -> Comp Env
        createEnvHelper [] [] = return M.empty
        createEnvHelper (v1 : l1s) ((v2, v2t) : l2s) =  do
            case v1 of
                NVar var_name ->
                    case v2t of
                        IntVar -> do
                                -- this will fail if var_name is not an integer
                            n <- getIntVar var_name
                            env2 <- createEnvHelper l1s l2s
                            return $ M.insert v2 (IntVal n) env2
                        ArrayVar -> do
                                        -- this will fail if var_name is not an array
                            (len, arr) <- getArrayVar var_name
                            env2 <- createEnvHelper l1s l2s
                            return $ M.insert v2 (ArrayVal len arr) env2
                IndexVar var_name e ->
                    case v2t of
                        IntVar -> do
                            (_, n) <- indexArrayAtExp var_name e
                            env2 <- createEnvHelper l1s l2s
                            return $ M.insert v2 (IntVal n) env2
                        ArrayVar -> raiseError $ BadVar "Trying to give int as var when array is expected"

updateOldEnv :: [ArgVar] -> [(VName, VarType)] -> Env -> Env -> Comp ()
updateOldEnv [] [] _ _ = return ()
updateOldEnv (v1 : l1s) ((v2, _) : l2s) o_env n_env =
    case M.lookup v2 n_env of
        Nothing -> raiseError $ BadVar "should not happen"
        Just val ->
            case v1 of
                NVar var_name ->
                    setVar var_name val >> updateOldEnv l1s l2s o_env n_env
                    --TODO: fix shadowing issues to do with this
                IndexVar var_name varexp -> do
                    (i, _) <- indexArrayAtExp var_name varexp
                    n <- getIntVal val (BadVal "should be int")
                    setValInArrayAtIndex var_name i n
                    -- setVar var_name 
                    updateOldEnv l1s l2s o_env n_env

setUpEnvComp :: [VarDecl] -> Comp Env
setUpEnvComp [] = return M.empty
setUpEnvComp (vd : vs) = do
            (var_name, val) <-
                    case vd of
                        IntV vn -> return (vn, IntVal 0)
                        AVar1 vn len -> 
                            if len < 1 then 
                                raiseError $ BadVar "array must be atleast length 1" 
                            else 
                                return (vn, ArrayVal len (replicate (fromIntegral len) 0))
                        AVar2 vn len arr ->
                            if len < 1 || length arr /= fromIntegral len then
                                raiseError $ BadVar "amount of input numbers must match array length"
                            else 
                                return (vn, ArrayVal len arr)
            cur <- setUpEnvComp vs
            return $ M.insert var_name val cur


setUpEnv :: Procedure -> Env
setUpEnv (Main vds _) = setUpEnvHelper vds
    where
        setUpEnvHelper [] = M.empty
        setUpEnvHelper (vd : vs) =
            let (var_name, val) =
                    case vd of
                        IntV vn -> (vn, IntVal 0)
                        AVar1 vn len -> if len < 1 then error "oof" else (vn, ArrayVal len (replicate (fromIntegral len) 0))
                        AVar2 vn len arr ->
                            if len < 1 || length arr /= fromIntegral len
                                then error "oof"
                            else (vn, ArrayVal len arr)
            in M.insert var_name val (setUpEnvHelper vs)
setUpEnv _ = error "only call setup env with main"

executeProgram :: Program -> Either Error Env
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
    in case runComp (executeStmt main_s) penv2 env of
        Left e -> Left e
        Right (_, env) -> Right env
