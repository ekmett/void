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

newtype Void = Void Void deriving (Eq,Ord,Show,Read)

-- | Since Void values are logically uninhabited, this witnesses the logical
-- reasoning tool of 'ex falso quodlibet'.
absurd :: Void -> a
absurd (Void a) = absurd a
