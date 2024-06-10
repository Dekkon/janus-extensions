import AST
import Interpreter

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map.Strict as M
import Data.Map(Map)
import Parser (parseProgram)
import Interpreter
main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests


tests :: TestTree
tests = testGroup "Tests" [
        parserTests, interpreterTests
    ]


parserTests :: TestTree
parserTests = testGroup "Parser tests" [
        parseExpressionsTests,
        fullParserTests
    ]

-- just use full parser to test expressions
-- assumes that main procedure, var_declaration and += statement is parsed correctly
pExpressionTest :: String -> Exp -> TestTree
pExpressionTest exp_str exp_result =
    testCase ("exp: " ++ exp_str) $ parseProgram ("procedure main() int a x += " ++ exp_str)
        @?= Right [("main",Main [IntV "a"] (SPluseq (IVar "x") exp_result))]


parseExpressionsTests :: TestTree
parseExpressionsTests = testGroup "Expressions" [
        pExpressionTest "42" (EConst (IntVal 42))
        ,pExpressionTest "-4" (EConst (IntVal (-4)))
        ,pExpressionTest "x" (EVar "x")
        ,pExpressionTest "4 + x" (EOp Plus (EConst (IntVal 4)) (EVar "x"))
        ,pExpressionTest "4 * x" (EOp Times (EConst (IntVal 4)) (EVar "x"))
        ,pExpressionTest "4 / x" (EOp Div (EConst (IntVal 4)) (EVar "x"))
        ,pExpressionTest "4 ^ x" (EOp XOr (EConst (IntVal 4)) (EVar "x"))
        ,pExpressionTest "4 && x" (EOp LAnd (EConst (IntVal 4)) (EVar "x"))
        ,pExpressionTest "4 || x" (EOp LOr (EConst (IntVal 4)) (EVar "x"))
        ,pExpressionTest "b + y[3-b]" (EOp Plus (EVar "b") (EArrIndex "y" (EOp Minus (EConst (IntVal 3)) (EVar "b"))))
        ,pExpressionTest "((((((((((((((((((((((((((((((((((((((((x))))))))))))))))))))))))))))))))))))))))" (EVar "x")
    ]


fullParserTests :: TestTree
fullParserTests = testGroup "Full Parser Tests" [
    testCase "larger program" $ parseProgram "procedure main() int x int y[4] = {1, 2, 3, 4}\
                \x -= 2 y ^= 3 foo <=> bar procedure oof(int foo, int bar[]) foo += 2"
        @?= Right [("main",Main [IntV "x",AVar2 "y" 4 [1,2,3,4]] 
                    (SSeq (SMinuseq (IVar "x") (EConst (IntVal 2))) 
                    (SSeq (SXoreq (IVar "y") (EConst (IntVal 3))) 
                    (SSwap (IVar "foo") (IVar "bar"))))),
                    ("oof",Procedure [("foo",IntVar),("bar",ArrayVar)] 
                    (SPluseq (IVar "foo") (EConst (IntVal 2))))]
    
    ,testCase "larger program, with comments" $ parseProgram "procedure main() // this is a comment \n int x\
                    \ int y[4] = {1, 2, 3, 4} x -= 2 y ^= 3 foo <=> bar procedure oof(int foo, int bar[]) foo += 2"
        @?= Right [("main",Main [IntV "x",AVar2 "y" 4 [1,2,3,4]] 
                    (SSeq (SMinuseq (IVar "x") (EConst (IntVal 2))) 
                    (SSeq (SXoreq (IVar "y") (EConst (IntVal 3))) 
                    (SSwap (IVar "foo") (IVar "bar"))))),
                    ("oof",Procedure [("foo",IntVar),("bar",ArrayVar)] 
                    (SPluseq (IVar "foo") (EConst (IntVal 2))))]

    ]

interpreterTests :: TestTree
interpreterTests = testGroup "Intepreter tests" [
        evalExpressionsTests, executeStmtTets
    ]

testCorrectExp :: String -> Exp -> Value -> PEnv -> Env -> TestTree
testCorrectExp teststr e res fenv env =
    testCase teststr $ runComp (evalExp e) fenv env @?= Right (res, env)

checkIfBadIndex err = 
    case err of 
        BadIndex _ -> return ()
        _ -> assertFailure ("Wrong error type, expected BadIndex, got: " ++ show err)

testFailingExp teststr e checkError fenv env = 
    testCase teststr $ let res =  runComp (evalExp e) fenv env 
                        in case res of
                            Right v -> assertFailure $ "expected expression to raise error instead: " ++ show v
                            Left err -> checkIfBadIndex err

evalExpressionsTests :: TestTree
evalExpressionsTests = testGroup "Eval exp tests" [
        testCorrectExp "index in bound: a[2]" (EArrIndex "a" (EConst (IntVal 2))) (IntVal 3) M.empty (M.fromList [("a", ArrayVal 3 [1, 2, 3])])
        ,testFailingExp "index out of bounds: a[3]" (EArrIndex "a" (EConst (IntVal 3))) checkIfBadIndex M.empty (M.fromList [("a", ArrayVal 3 [1, 2, 3])])
        ,testCorrectExp "op +: 3 + 4" (EOp Plus (EConst (IntVal 3)) (EConst (IntVal 4))) (IntVal 7) M.empty M.empty
        ,testCorrectExp "op -: 3 - 4" (EOp Minus (EConst (IntVal 3)) (EConst (IntVal 4))) (IntVal (-1)) M.empty M.empty
        ,testCorrectExp "op *: 3 * 4" (EOp Times (EConst (IntVal 3)) (EConst (IntVal 4))) (IntVal 12) M.empty M.empty
        ,testCorrectExp "op /: 12 / 4" (EOp Div (EConst (IntVal 12)) (EConst (IntVal 4))) (IntVal 3) M.empty M.empty
        ,testCorrectExp "op %: 10 % 4" (EOp Mod (EConst (IntVal 10)) (EConst (IntVal 4))) (IntVal 2) M.empty M.empty
        ,testCorrectExp "op &: 15 & 3" (EOp BAnd (EConst (IntVal 15)) (EConst (IntVal 3))) (IntVal 3) M.empty M.empty
        ,testCorrectExp "op |: 5 | 3" (EOp BOr (EConst (IntVal 4)) (EConst (IntVal 3))) (IntVal 7) M.empty M.empty
        ,testCorrectExp "op ^: 5 ^ 3" (EOp XOr (EConst (IntVal 5)) (EConst (IntVal 3))) (IntVal 6) M.empty M.empty
        ,testCorrectExp "op =: 5 = 3" (EOp Eq (EConst (IntVal 5)) (EConst (IntVal 3))) (BoolVal False) M.empty M.empty
        ,testCorrectExp "op =: 5 = 5" (EOp Eq (EConst (IntVal 5)) (EConst (IntVal 5))) (BoolVal True) M.empty M.empty
        ,testCorrectExp "op &&: false && true" (EOp LAnd (EConst (BoolVal False)) (EConst (BoolVal True))) (BoolVal False) M.empty M.empty
        ,testCorrectExp "op ||: false || true" (EOp LOr (EConst (BoolVal False)) (EConst (BoolVal True))) (BoolVal True) M.empty M.empty
    ]
testStatement :: String -> Stmt -> Env -> PEnv -> Env -> TestTree
testStatement teststr stmt res fenv env = 
    testCase teststr $ runComp (executeStmt stmt) fenv env @?= Right ((), res)

executeStmtTets :: TestTree
executeStmtTets = testGroup "execute statement tests" [
        testGroup "single statements" [
            testStatement "a += 3" (SPluseq (IVar "a") (EConst (IntVal 3))) 
                                    (M.fromList [("a", IntVal 7)]) --assumed result env
                                    M.empty (M.fromList [("a", IntVal 4)]) --starting env

            ,testStatement "a ^= 3" (SXoreq (IVar "a") (EConst (IntVal 3))) 
                                    (M.fromList [("a", IntVal 4)]) --assumed result env
                                    M.empty (M.fromList [("a", IntVal 7)]) --starting env

            ,testStatement "a[0] += 1" (SPluseq (AVar "a" (EConst (IntVal 0))) (EConst (IntVal 1))) 
                                    (M.fromList [("a", ArrayVal 4 [1, 0, 0, 0])]) --assumed result env
                                    M.empty (M.fromList [("a", ArrayVal 4 [0, 0, 0, 0])]) --starting env

            ,testStatement "a[1] -= 2" (SMinuseq (AVar "a" (EConst (IntVal 1))) (EConst (IntVal 2))) 
                                    (M.fromList [("a", ArrayVal 4 [0, -2, 0, 0])]) --assumed result env
                                    M.empty (M.fromList [("a", ArrayVal 4 [0, 0, 0, 0])]) --starting env
            
            ,testStatement "a <=> b" (SSwap (IVar "a") (IVar "b")) 
                                    (M.fromList [("a", IntVal 10), ("b", IntVal 3)]) --assumed result env
                                    M.empty (M.fromList [("a", IntVal 3), ("b", IntVal 10)]) --starting env
            
            ,testStatement "a <=> b[1]" (SSwap (IVar "a") (AVar "b" (EConst (IntVal 1)))) 
                                    (M.fromList [("a", IntVal 10), ("b", ArrayVal 2 [1, 3])]) --assumed result env
                                    M.empty (M.fromList [("a", IntVal 3), ("b", ArrayVal 2 [1, 10])]) --starting env
            
            ,testStatement "b[1] <=> a" (SSwap (AVar "b" (EConst (IntVal 1))) (IVar "a")) 
                                    (M.fromList [("a", IntVal 10), ("b", ArrayVal 2 [1, 3])]) --assumed result env
                                    M.empty (M.fromList [("a", IntVal 3), ("b", ArrayVal 2 [1, 10])]) --starting env
            
            ,testStatement "a[0] <=> a[1]" (SSwap (AVar "a" (EConst (IntVal 0))) (AVar "a" (EConst (IntVal 1)))) 
                                    (M.fromList [("a", ArrayVal 2 [10, 1])]) --assumed result env
                                    M.empty (M.fromList [("a", ArrayVal 2 [1, 10])]) --starting env
        ]
        ,testGroup "statement sequences" [
            testStatement "a += 3 a += 4" (SSeq (SPluseq (IVar "a") (EConst (IntVal 3))) (SPluseq (IVar "a") (EConst (IntVal 4)))) 
                                    (M.fromList [("a", IntVal 11)]) --assumed result env
                                    M.empty (M.fromList [("a", IntVal 4)]) --starting env
            ,testStatement "a[0] += 1 a[1] += 2" (SSeq (SPluseq (AVar "a" (EConst (IntVal 0))) (EConst (IntVal 1))) (SPluseq (AVar "a" (EConst (IntVal 1))) (EConst (IntVal 2))))
                                    (M.fromList [("a", ArrayVal 4 [1, 2, 0, 0])]) --assumed result env
                                    M.empty (M.fromList [("a", ArrayVal 4 [0, 0, 0, 0])]) --starting env
        ]
        ,testGroup "composite statements" [
            testStatement "if a = 2 then a += 2 else a += 2 fi a = 4: a starts as 2"  (SIfThenElse (EOp Eq (EVar "a") (EConst (IntVal 2))) (SPluseq (IVar "a") (EConst (IntVal 2))) (SPluseq (IVar "a") (EConst (IntVal 2))) (EOp Eq (EVar "a") (EConst (IntVal 4)))) 
                                    (M.fromList [("a", IntVal 4)]) --assumed result env
                                    M.empty (M.fromList [("a", IntVal 2)]) --starting env
            ,testStatement "if a = 2 then a += 2 else a += 2 fi a = 4: a starts as 4"  (SIfThenElse (EOp Eq (EVar "a") (EConst (IntVal 2))) (SPluseq (IVar "a") (EConst (IntVal 2))) (SPluseq (IVar "a") (EConst (IntVal 2))) (EOp Eq (EVar "a") (EConst (IntVal 4)))) 
                                    (M.fromList [("a", IntVal 6)]) --assumed result env
                                    M.empty (M.fromList [("a", IntVal 4)]) --starting env

        ]
    ]