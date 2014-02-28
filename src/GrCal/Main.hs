-- | The main module simply kick-starts the program.
module Main where

data Expr = Expr | Stmt

main :: IO ()
main = putStrLn "Hello, GrCal"
