-- This module defines a simple command line interface for the Boa
-- interpreter.  If your solution is correct, this module should just
-- work.
module Main (main) where

import AST
import Parser
import Interpreter

import System.Exit (die)
import System.Environment (getArgs)
import qualified Data.Map.Strict as M
import Data.Map(Map)

run :: Program -> IO ()
run p =
  do let res = executeProgram p
    --  mapM_ putStrLn out
     case res of
       Left e -> print e
       Right env -> mapM_ print (M.toList env)

main :: IO ()
main = do args <- getArgs
          case args of
            -- ["-i", file] -> do
            --   s <- readFile file
            --   run $ read s
            -- ["-p", file] -> do
            --   s <- readFile file
            --   case parseString s of
            --     Left e -> putStrLn $ "*** Parse error: " ++ show e
            --     Right p -> putStrLn $ show p
            [file] -> do
              s <- readFile file
              case parseProgram s of
                Left e -> putStrLn $ "*** Parse error: " ++ show e
                Right p -> run p
            _ ->
              die "Usage:\n\
                    \  janus PROGRAM.janus"
