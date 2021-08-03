-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- Copyright   :  (c) 2021 Brian W Bush
-- License     :  MIT
--
-- Maintainer  :  Brian W Bush <code@functionally.io>
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Random-number generator.
--
-----------------------------------------------------------------------------


{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}


module Plutus.OnChain.Random (
-- * Types
  RandomGenerator
-- * Functions
, makeRandomGenerator
, nextInteger64
) where


import PlutusTx.Prelude

import Plutus.OnChain.Bits (xor)

import qualified Prelude


-- | A splittable random-number generator. See <https://doi.org/10.1145/2660193.2660195> and the "splitmix" Haskell package.
data RandomGenerator =
  RandomGenerator
  {
    seed  :: Integer -- ^ The seed.
  , gamma :: Integer -- ^ The gamma constant.
  }
    deriving (Prelude.Show)


{-# INLINABLE makeRandomGenerator #-}

-- | Initialize the random-number generator.
makeRandomGenerator :: Integer         -- ^ The seed.
                    -> RandomGenerator -- ^ The random-number generator.
makeRandomGenerator seed =
  RandomGenerator
  {
    seed = truncate64 seed
  , gamma = goldenGamma
  }
    where
      goldenGamma = 0x9e3779b97f4a7c15


{-# INLINABLE nextInteger64 #-}

-- | Return the next 64-bit non-negative integer.
nextInteger64 :: RandomGenerator            -- ^ The random-number generator.
              -> (Integer, RandomGenerator) -- ^ The random number and the modified random-number generator.
nextInteger64 rg =
  let
    rg' = nextSeed rg
  in
    (mix64 $ seed rg', rg')


{-# INLINABLE nextSeed #-}
      
-- | Update the random-number generator with its next seed.
nextSeed :: RandomGenerator -- ^ The random-number generator.
         -> RandomGenerator -- ^ The modified random-number generator.
nextSeed rg@RandomGenerator{..} =
  rg
  {
    seed = truncate64 $ seed + gamma
  }


{-# INLINABLE mix64 #-}

-- | Mix bits of a 64-bit non-negative integer.
mix64 :: Integer -- ^ Before mixing.
      -> Integer -- ^ After mixing.
mix64 z =
  let
    xorShift w = w `xor` shift33 w
    z'  = truncate64 $ xorShift z  * 0xff51afd7ed558ccd
    z'' = truncate64 $ xorShift z' * 0xc4ceb9fe1a85ec53
  in
    truncate64 $ xorShift z''


{-# INLINABLE shift33 #-}

-- | Shift a non-negative integer to the right by 33 bits.
shift33 :: Integer -- ^ The integer.
        -> Integer -- ^ The right-shifted integer.
shift33 = (`divide` 0x200000000)


{-# INLINABLE truncate64 #-}

-- | Discard all but the last 64 bits of a non-negative integer.
truncate64 :: Integer -- ^ The integer.
           -> Integer -- ^ The truncated integer.
truncate64 = (`modulo` 0x10000000000000000)
