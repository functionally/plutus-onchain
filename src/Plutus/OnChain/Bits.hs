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
-- | Bit operations.
--
-----------------------------------------------------------------------------


{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy       #-}


module Plutus.OnChain.Bits (
-- * Functions
  xor
, odd
, even
) where


import PlutusTx.Prelude hiding (even)


{-# INLINABLE xor #-}

-- | Compute the bitwise exclusive-or of two non-negative integers.
xor :: Integer -- ^ The first integer.
    -> Integer -- ^ The second integer.
    -> Integer -- ^ The bitwise exclusive-or.
xor = xor' 1 0
  where
    xor' :: Integer
         -> Integer
         -> Integer
         -> Integer
         -> Integer
    xor' m z x y
      | x == 0           = z + m * y
      | y == 0           = z + m * x
      | even x == even y = xor' (2 * m)  z      (x `divide` 2) (y `divide` 2) 
      | otherwise        = xor' (2 * m) (z + m) (x `divide` 2) (y `divide` 2)


{-# INLINABLE even #-}

-- | Test whether a non-negative integer is even.
even :: Integer -- ^ The integer.
     -> Bool    -- ^ Whether the integer is even.
even x = x `modulo` 2 == 0


{-# INLINABLE odd #-}

-- | Test whether a non-negative integer is odd.
odd :: Integer -- ^ The integer.
    -> Bool    -- ^ Whether the integer is odd.
odd x = x `modulo` 2 == 1
