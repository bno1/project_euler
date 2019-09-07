module P15
  ( solveP15
  ) where

import Common
import Data.Word


-- If you write on each vertex the numbers of paths going through it a pascal
-- triangle starts to emerge. Let N(x) be the numbers of paths going through
-- vertex x. We have:
--   N(x) = N(top(x)) + N(left(x))
-- To obtain the final result imagine exteding the triangle until the grid is
-- entirely contained by it. The number of paths reaching the bottom right
-- corner is the value of the pascal triangle at the bottom right position.
-- That is at height 2n and position n where n is the number of edges.
solveP15 :: Word64
solveP15 = combinations (2 * n) n
  where n = 20
