{-# LANGUAGE FlexibleContexts, BangPatterns #-}

module Sieve where

import Common

import Data.Word
import Control.Monad (when, forM_)
import Data.Foldable (toList)
import Control.Monad.ST.Strict (ST, runST)
import qualified Data.Array.MArray as MA
import qualified Data.Array.ST as STA
import qualified Data.Sequence as S
import qualified Control.Monad.State as St

data Sieve = Sieve { sivPrimes :: !(S.Seq Word64)
                   , sivBufSize :: !Word64
                   , sivPosition :: !Word64
                   } deriving (Show)

newSieve :: Word64 -> Sieve
newSieve stepSize = Sieve (S.singleton 2) stepSize 3

runSieve :: St.State Sieve ()
runSieve = St.modify $ \(Sieve primes bufSize pos) -> let
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

nThPrime :: Int -> St.State Sieve Word64
nThPrime n = do
  (Sieve primes _ _) <- St.get
  if n < S.length primes then return $ S.index primes n
  else runSieve >> nThPrime n

runSieveUntil :: Word64 -> St.State Sieve ()
runSieveUntil n = do
  (Sieve _ _ pos) <- St.get
  when (pos <= n) $ runSieve >> runSieveUntil n

sivPrimesList :: Sieve -> [Word64]
sivPrimesList = toList . sivPrimes

sivFactorize :: Word64 -> St.State Sieve (S.Seq (Word64, Word))
sivFactorize n = let
    nlim = iSqrt n
    factorize k next = do
      (v, lim, factors) <- St.get
      when (k <= lim) $ do
        let (d, c) = countFactorHelper k v 0
        when (c /= 0) $ St.put (d, iSqrt d, factors S.|> (k, c))

        next
  in do
    runSieveUntil nlim

    comp <- St.gets $ foldr factorize (return ()) . sivPrimes

    let (d, _, factors) = St.execState comp (n, nlim, S.empty)

    return $ if d /= 1 then factors S.|> (d, 1) else factors
