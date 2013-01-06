{-# LANGUAGE Unsafe #-}
module Data.Void.Unsafe where

import Data.Void

import Unsafe.Coerce

-- | If 'Void' is uninhabited than any 'Functor' that holds only values of the type 'Void'
-- is holding no values.
--
-- This is only safe for valid 'Functor's, and may not work for compilers other than GHC.
unsafeVacuous :: Functor f => f Void -> f a
unsafeVacuous = unsafeCoerce

-- | If 'Void' is uninhabited then any 'Monad' that holds values of type 'Void'
-- is holding no values.
--
-- This is only safe for valid 'Monad's, and may not work for compilers other than GHC.
unsafeVacuousM :: Monad m => m Void -> m a
unsafeVacuousM = unsafeCoerce