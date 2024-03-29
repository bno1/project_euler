module Main where

import Lib
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs

  case args of
    [] -> putStrLn "Too few arguments"
    [arg] -> do
      let n = read arg
      putStrLn $ "Running problem #" ++ show n
      runProblem n >>= putStrLn
    _ -> putStrLn "Too many arguments"
