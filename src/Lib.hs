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
import P19 (solveP19)
import P20 (solveP20)
import P21 (solveP21)
import P22 (solveP22)

runProblem :: Int -> IO String
runProblem 1 = return $ show solveP1
runProblem 2 = return $ show solveP2
runProblem 3 = return $ show solveP3
runProblem 4 = return $ show solveP4
runProblem 5 = return $ show solveP5
runProblem 6 = return $ show solveP6
runProblem 7 = return $ show solveP7
runProblem 8 = return $ show solveP8
runProblem 9 = return $ show solveP9
runProblem 10 = return $ show solveP10
runProblem 11 = return $ show solveP11
runProblem 12 = return $ show solveP12
runProblem 13 = return solveP13
runProblem 14 = return $ show solveP14
runProblem 15 = return $ show solveP15
runProblem 16 = return $ show solveP16
runProblem 17 = return $ show solveP17
runProblem 18 = return $ show solveP18
runProblem 19 = return $ show solveP19
runProblem 20 = return $ show solveP20
runProblem 21 = return $ show solveP21
runProblem 22 = show <$> solveP22
runProblem _ = return "Unknown problem"
