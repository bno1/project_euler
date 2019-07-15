module Lib
    ( runProblem
    ) where

import P1 (solveP1)

runProblem :: Int -> String
runProblem 1 = solveP1
runProblem _ = "Unknown problem"
