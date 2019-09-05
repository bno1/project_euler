module P14
  ( solveP14
  ) where

import Data.Word
import Control.Monad.ST.Strict (ST)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

type MTable s = VUM.STVector s Word32
type Table = VU.Vector Word32

runN :: MTable s -> Word32 -> Word32 -> ST s Word32
runN table m n = do
  let idx = fromIntegral n
  let n' = if even n then (n `quot` 2) else (3 * n + 1)
  let m' = m + 1

  if idx >= VUM.length table then runN table m' n'
  else do
    v <- VUM.unsafeRead table idx
    if v > 0 then return (v + m')
    else do
      cnt <- runN table 0 n'
      VUM.unsafeWrite table idx cnt
      return (cnt + m')

runUntil :: Word32 -> Table
runUntil limit = VU.create $ do
  table <- VUM.replicate (fromIntegral limit + 1) 0
  VUM.unsafeWrite table 1 1

  mapM_ (runN table 0) [2..limit]

  return table

solveP14 :: Int
solveP14 = VU.maxIndex $ runUntil 1000000
