module Test.Main where

import Prelude
import Data.Foldable (for_)
import Data.Array as Array
import Data.ModularArithmetic.Primality (isPrime)
import Control.Monad.Eff.Exception (throw)
import Control.Monad.Eff.Console (log)
import LegendreSymbols as LS

oddPrimesUpTo n = Array.filter isPrime (Array.range 3 n)

main = do
  for_ (oddPrimesUpTo 250) \p -> do
    log ("Testing: " <> show p)
    for_ (Array.range 1 p) \a -> do
       when (LS.legendreSymbol a p /= LS.legendreSymbol' a p)
        (throw ("Did not match; a = " <> show a <> ", p = " <> show p))
  log "Tests passed"
