module P8 where

import Data.Word
import Data.Maybe (fromMaybe)
import Data.Char (digitToInt)

queueLimit :: Int
queueLimit = 13

input :: String
input = "73167176531330624919225119674426574742355349194934\
\96983520312774506326239578318016984801869478851843\
\85861560789112949495459501737958331952853208805511\
\12540698747158523863050715693290963295227443043557\
\66896648950445244523161731856403098711121722383113\
\62229893423380308135336276614282806444486645238749\
\30358907296290491560440772390713810515859307960866\
\70172427121883998797908792274921901699720888093776\
\65727333001053367881220235421809751254540594752243\
\52584907711670556013604839586446706324415722155397\
\53697817977846174064955149290862569321978468622482\
\83972241375657056057490261407972968652414535100474\
\82166370484403199890008895243450658541227588666881\
\16427171479924442928230863465674813919123162824586\
\17866458359124566529476545682848912883142607690042\
\24219022671055626321111109370544217506941658960408\
\07198403850962455444362981230987879927244284909188\
\84580156166097919133875499200524063689912560717606\
\05886116467109405077541002256983155200055935729725\
\71636269561882670428252483600823257530420752963450"

data Queue = Queue { qProd :: Word64  -- product of non-zero numbers in the
                                      -- queue
                   , qZeros :: Word64 -- number of zeros in queue
                   -- classic queue implementation using two stacks
                   , qPushList :: [Word8]
                   , qPopList :: [Word8]
                   } deriving (Show)

newQueue :: Queue
newQueue = Queue 1 0 [] []

-- push elem to queue implemented with two stacks
queuePush :: ([a], [a]) -> a -> (Maybe a, [a], [a])
queuePush (pushl, popl) v
  | length pushl + length popl < queueLimit = (Nothing, v:pushl, popl)
  | not (null popl) = (Just $ head popl, v:pushl, tail popl)
  | otherwise = (Just $ head popl', [], tail popl')
  where
    popl' = reverse (v:pushl)

qPush :: Queue -> Word8 -> Queue
qPush (Queue p z pushl popl) n = Queue p' z' pushl' popl'
  where
    (xm, pushl', popl') = queuePush (pushl, popl) n
    -- popped element
    x = fromMaybe 0 xm
    -- p * new elem / popped elem
    p' = p * max 1 (fromIntegral n) `quot` max 1 (fromIntegral x)
    -- +1 if new element is 0, -1 if popped element is 0
    z' = z + (if n == 0 then 1 else 0) - (if xm == Just 0 then 1 else 0)

qGetProd :: Queue -> Word64
qGetProd (Queue p z pushl popl)
  | z > 0 = 0
  | length pushl + length popl < queueLimit = 0
  | otherwise = p

solveP8 :: Word64
solveP8 = maximum $ map qGetProd $ scanl qPush newQueue digits
  where digits = map (fromIntegral . digitToInt) input
