{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
-- | Existential types and Eliminators
module TWT.Chapter7
  where


data HasShow where
  HasShow :: Show t => t -> HasShow
  --HasShow :: t -> HasShow

elimHasShow :: (forall a. Show a => a -> r) -> HasShow -> r
elimHasShow f (HasShow a) = f a
  
instance Show HasShow where
  show = elimHasShow show

