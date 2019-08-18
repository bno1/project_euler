{-# LANGUAGE FlexibleContexts, BangPatterns #-}

module Sieve where

import Data.Word
import Control.Monad (when, forM_)
import Data.Foldable (toList)
import qualified Data.Sequence as S
import Control.Monad.ST.Strict (ST, runST)
import qualified Data.Array.MArray as MA
import qualified Data.Array.ST as STA

data Sieve = Sieve { sivPrimes :: !(S.Seq Word64)
                   , sivBufSize :: !Word64
                   , sivPosition :: !Word64
                   } deriving (Show)

newSieve :: Word64 -> Sieve
newSieve stepSize = Sieve (S.singleton 2) stepSize 3

runSieve :: Sieve -> Sieve
runSieve (Sieve primes bufSize pos) = let
    removeMults m !p !i = when (i < bufSize) $ do
        MA.writeArray m i False
        removeMults m p (i + p)

    walkArray m !i !ps
      | i < bufSize = do
          v <- MA.readArray m i
          if v then do
            let p = i * 2 + pos
            removeMults m p $ i + p
            walkArray m (i + 1) (ps S.|> p)
          else
            walkArray m (i + 1) ps
      | otherwise = return ps

  in runST $ do
    m <- MA.newArray (0, bufSize) True :: ST s (STA.STUArray s Word64 Bool)

    -- return the position of the next multiple of p starting from `position`
    -- magical formula
    let align p = ((pos `rem` p) * (p `quot` 2)) `rem` p

    forM_ (S.drop 1 primes) $ \p -> removeMults m p (align p)

    newPrimes <- walkArray m 0 S.empty

    return $ Sieve (primes S.>< newPrimes) bufSize (pos + 2 * bufSize)

nThPrime :: Int -> Sieve -> (Word64, Sieve)
nThPrime n sieve@(Sieve primes _ _)
  | n < S.length primes = (S.index primes n, sieve)
  | otherwise = nThPrime n $ runSieve sieve

runSieveUntil :: Sieve -> Word64 -> Sieve
runSieveUntil s n =
  head $ dropWhile (\s' -> sivPosition s' <= n) $ iterate runSieve s

sivPrimesList :: Sieve -> [Word64]
sivPrimesList = toList . sivPrimes
