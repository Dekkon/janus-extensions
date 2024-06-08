import JanusAST

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests


tests :: TestTree
tests = testGroup "Tests" [
    
    ]


parserTests :: TestTree
parserTests = testGroup "Parser tests" [
    
    ]