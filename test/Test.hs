import AST

import Test.Tasty
import Test.Tasty.HUnit
import Parser (parseProgram)

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests


tests :: TestTree
tests = testGroup "Tests" [
        parserTests
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
        @?= Right [Main [IntV "a"] (SPluseq (IVar "x") exp_result)]


parseExpressionsTests :: TestTree
parseExpressionsTests = testGroup "Expressions" [
        pExpressionTest "42" (EConst (IntVal 42))
        ,pExpressionTest "-4" (EConst (IntVal (-4)))
        ,pExpressionTest "x" (EVar "x")
        ,pExpressionTest "4 + x" (EOp Plus (EConst (IntVal 4)) (EVar "x"))
        ,pExpressionTest "4 * x" (EOp Times (EConst (IntVal 4)) (EVar "x"))
        ,pExpressionTest "4 / x" (EOp Div (EConst (IntVal 4)) (EVar "x"))
        ,pExpressionTest "4 ^ x" (EOp XOr (EConst (IntVal 4)) (EVar "x"))
        ,pExpressionTest "4 & x" (EOp LAnd (EConst (IntVal 4)) (EVar "x"))
        ,pExpressionTest "4 | x" (EOp LOr (EConst (IntVal 4)) (EVar "x"))
        ,pExpressionTest "b + y[3-b]" (EOp Plus (EVar "b") (EArrIndex "y" (EOp Minus (EConst (IntVal 3)) (EVar "b"))))
        ,pExpressionTest "((((((((((((((((((((((((((((((((((((((((x))))))))))))))))))))))))))))))))))))))))" (EVar "x")
    ]


fullParserTests :: TestTree
fullParserTests = testGroup "Full Parser Tests" [
    testCase "larger program" $ parseProgram "procedure main() int x int y[4] = {1, 2, 3, 4}\
                \x -= 2 y ^= 3 foo <=> bar procedure oof(int foo, int bar[]) foo += 2"
        @?= Right [Main [IntV "x",AVar2 "y" (EConst (IntVal 4)) [1,2,3,4]] 
                    (SSeq (SMinuseq (IVar "x") (EConst (IntVal 2))) 
                    (SSeq (SXoreq (IVar "y") (EConst (IntVal 3))) 
                            (SSwap (IVar "foo") (IVar "bar")))), 
                   Procedure "oof" [("foo",IntVar),("bar",ArrayVar)] 
                    (SPluseq (IVar "foo") (EConst (IntVal 2)))]
    ,testCase "larger program, with comments" $ parseProgram "procedure main() // this is a comment \n int x\
                    \ int y[4] = {1, 2, 3, 4} x -= 2 y ^= 3 foo <=> bar procedure oof(int foo, int bar[]) foo += 2"
        @?= Right [Main [IntV "x",AVar2 "y" (EConst (IntVal 4)) [1,2,3,4]] 
                    (SSeq (SMinuseq (IVar "x") (EConst (IntVal 2))) 
                    (SSeq (SXoreq (IVar "y") (EConst (IntVal 3))) 
                            (SSwap (IVar "foo") (IVar "bar")))), 
                   Procedure "oof" [("foo",IntVar),("bar",ArrayVar)] 
                    (SPluseq (IVar "foo") (EConst (IntVal 2)))]

    ]