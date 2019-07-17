module Lib
    ( runProblem
    ) where

import P1 (solveP1)
import P2 (solveP2)
import P3 (solveP3)
import P4 (solveP4)
import P5 (solveP5)
import P6 (solveP6)

runProblem :: Int -> String
runProblem 1 = show solveP1
runProblem 2 = show solveP2
runProblem 3 = show solveP3
runProblem 4 = show solveP4
runProblem 5 = show solveP5
runProblem 6 = show solveP6
runProblem _ = "Unknown problem"
