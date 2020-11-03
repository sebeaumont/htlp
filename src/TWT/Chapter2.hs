{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module TWT.Chapter2 where

import GHC.TypeLits

-- | exercise 2.4-i
type family Not (x :: Bool) :: Bool where
  Not 'True = 'False
  Not 'False = 'True
