{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

#ifdef LANGUAGE_DeriveDataTypeable
{-# LANGUAGE DeriveDataTypeable #-}
#endif

#ifdef LANGUAGE_DeriveGeneric
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
#endif

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2008-2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Data.Void
  ( Void
  , absurd
  , vacuous
  , vacuousM
  ) where

import Control.Monad (liftM)
import Data.Ix
import Data.Hashable
import Data.Semigroup (Semigroup(..))

#ifdef LANGUAGE_DeriveDataTypeable
import Data.Data
#endif

#ifdef LANGUAGE_DeriveGeneric
import GHC.Generics
#endif

#if MIN_VERSION_base(4,0,0)
import Control.Exception
#endif

-- | A logically uninhabited data type.
#if __GLASGOW_HASKELL__ < 700
data Void = Void !Void
#else
newtype Void = Void Void
#endif
#ifdef LANGUAGE_DeriveDataTypeable
  deriving (Data, Typeable)
#endif

#ifdef LANGUAGE_DeriveGeneric
deriving instance Generic Void
#endif

instance Eq Void where
  _ == _ = True

instance Hashable Void where
  hashWithSalt _ = absurd

instance Ord Void where
  compare _ _ = EQ

instance Show Void where
  showsPrec _ = absurd

-- | Reading a 'Void' value is always a parse error, considering 'Void' as
-- a data type with no constructors.
instance Read Void where
  readsPrec _ _ = []

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

#if MIN_VERSION_base(4,0,0)
instance Exception Void
#endif
