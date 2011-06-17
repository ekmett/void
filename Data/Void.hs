{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Void
-- Copyright   :  (C) 2008-2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Data.Void (Void, absurd) where

import Data.Semigroup (Semigroup(..))
import Data.Ix
#ifdef GLASGOW_HASKELL > 610
import Data.Data
import Data.Typeable
#endif

#ifdef GLASGOW_HASKELL < 700
data Void = Void !Void 
#else
newtype Void = Void Void
#endif
  deriving 
  ( Eq, Ord, Show, Read
#ifdef GLASGOW_HASKELL > 610
  , Data, Typeable
#endif
  )

-- | Since Void values are logically uninhabited, this witnesses the logical
-- reasoning tool of 'ex falso quodlibet'.
absurd :: Void -> a
absurd (Void a) = absurd a

instance Semigroup Void where
  a <> _ = a

instance Ix Void where
  range _ = []
  index _ = absurd
  inRange _ = absurd
  rangeSize _ = 0 
