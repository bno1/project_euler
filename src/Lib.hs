module Lib
    ( runProblem
    ) where

import P1 (solveP1)
import P2 (solveP2)

runProblem :: Int -> String
runProblem 1 = show solveP1
runProblem 2 = show solveP2
runProblem _ = "Unknown problem"
