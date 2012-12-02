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

import Data.Ix
import Control.Monad (liftM)
import Data.Semigroup (Semigroup(..))

#ifdef LANGUAGE_DeriveDataTypeable
import Data.Data
#endif

-- | A logically uninhabited data type.
#if __GLASGOW_HASKELL__ < 700
data Void = Void !Void
#else
newtype Void = Void Void
#endif
  deriving
  ( Show, Read
#ifdef LANGUAGE_DeriveDataTypeable
  , Data, Typeable
#endif
  )

instance Eq Void where
  _ == _ = True

instance Ord Void where
  compare _ _ = EQ

-- | Since 'Void' values logically don't exist, this witnesses the logical
-- reasoning tool of \"ex falso quodlibet\".
absurd :: Void -> a
absurd a = a `seq` spin a where
   spin (Void b) = spin b

-- | If 'Void' is uninhabited then any 'Functor' that holds only values of type 'Void'
-- is holding no values.
vacuous :: Functor f => f Void -> f a
vacuous = fmap absurd

-- | If 'Void' is uninhabited then any 'Monad' that holds values of type 'Void'
-- is holding no values.
vacuousM :: Monad m => m Void -> m a
vacuousM = liftM absurd

instance Semigroup Void where
  a <> _ = a
  times1p _ a = a

instance Ix Void where
  range _ = []
  index _ = absurd
  inRange _ = absurd
  rangeSize _ = 0
