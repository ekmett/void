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
module Data.Void (Void, absurd, vacuous, vacuousM) where

import Data.Semigroup (Semigroup(..))
import Data.Ix

#ifdef LANGUAGE_DeriveDataTypeable
import Data.Data
#endif

#ifdef __GLASGOW_HASKELL__
import Unsafe.Coerce
#else
import Control.Monad (liftM)
#endif

-- | A logically uninhabited data type.
#if __GLASGOW_HASKELL__ < 700
data Void = Void !Void
#else
newtype Void = Void Void
#endif
  deriving
  ( Eq, Ord, Show, Read
#ifdef LANGUAGE_DeriveDataTypeable
  , Data, Typeable
#endif
  )

-- | Since Void values are logically uninhabited, this witnesses the logical
-- reasoning tool of 'ex falso quodlibet'.
absurd :: Void -> a
absurd a = a `seq` spin a where
   spin (Void b) = spin b

-- | If 'Void' is uninhabited then any 'Functor' that holds values of type 'Void'
-- is holding no values.
vacuous :: Functor f => f Void -> f a
#ifdef __GLASGOW_HASKELL__
vacuous = unsafeCoerce
#else
-- other haskell compilers are free to use less homogeneous representations
vacuous = fmap absurd
#endif

-- | If 'Void' is uninhabited then any 'Monad' that holds values of type 'Void'
-- is holding no values.
vacuousM :: Monad m => m Void -> m a
#ifdef __GLASGOW_HASKELL__
vacuousM = unsafeCoerce
#else
vacuousM = liftM absurd
#endif

instance Semigroup Void where
  a <> _ = a
  times1p _ a = a

instance Ix Void where
  range _ = []
  index _ = absurd
  inRange _ = absurd
  rangeSize _ = 0
