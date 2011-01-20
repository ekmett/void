-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Void
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Data.Void where

newtype Void = Void Void deriving (Eq,Ord,Show,Read)

void :: Void -> a
void (Void a) = void a
