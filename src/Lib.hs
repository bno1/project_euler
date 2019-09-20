module Lib
    ( runProblem
    ) where

import P1 (solveP1)
import P2 (solveP2)
import P3 (solveP3)
import P4 (solveP4)
import P5 (solveP5)
import P6 (solveP6)
import P7 (solveP7)
import P8 (solveP8)
import P9 (solveP9)
import P10 (solveP10)
import P11 (solveP11)
import P12 (solveP12)
import P13 (solveP13)
import P14 (solveP14)
import P15 (solveP15)
import P16 (solveP16)
import P17 (solveP17)
import P18 (solveP18)

runProblem :: Int -> String
runProblem 1 = show solveP1
runProblem 2 = show solveP2
runProblem 3 = show solveP3
runProblem 4 = show solveP4
runProblem 5 = show solveP5
runProblem 6 = show solveP6
runProblem 7 = show solveP7
runProblem 8 = show solveP8
runProblem 9 = show solveP9
runProblem 10 = show solveP10
runProblem 11 = show solveP11
runProblem 12 = show solveP12
runProblem 13 = solveP13
runProblem 14 = show solveP14
runProblem 15 = show solveP15
runProblem 16 = show solveP16
runProblem 17 = show solveP17
runProblem 18 = show solveP18
runProblem _ = "Unknown problem"
