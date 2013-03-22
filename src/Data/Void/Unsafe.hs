{-# LANGUAGE CPP #-}
#if !defined(SAFE) && defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 704
#define UNSAFE
{-# LANGUAGE Unsafe #-}
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
module Data.Void.Unsafe
  ( unsafeVacuous
  , unsafeVacuousM
  ) where

import Data.Void

#ifdef UNSAFE
import Unsafe.Coerce
#endif

-- | If 'Void' is uninhabited than any 'Functor' that holds only values of the type 'Void'
-- is holding no values.
--
-- This is only safe for valid functors that do not perform GADT-like analysis on the argument.
unsafeVacuous :: Functor f => f Void -> f a
#ifdef UNSAFE
unsafeVacuous = unsafeCoerce
#else
unsafeVacuous = fmap absurd
#endif

-- | If 'Void' is uninhabited then any 'Monad' that holds values of type 'Void'
-- is holding no values.
--
-- This is only safe for valid monads that do not perform GADT-like analysis on the argument.
unsafeVacuousM :: Monad m => m Void -> m a
#ifdef UNSAFE
unsafeVacuousM = unsafeCoerce
#else
unsafeVacuousM m = m >>= return . absurd
#endif
