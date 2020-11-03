module Listy
  ( Listy
  , toList
  , fromList
  ) where

import Control.Applicative
import Data.Either

newtype Listy a = Listy { toList :: [a] }
  deriving (Show, Eq)

fromList = Listy

instance Semigroup (Listy a) where
  (<>) (Listy x) (Listy y) = fromList $ x <> y
  
instance Monoid (Listy a) where
  mempty = fromList []  

instance Functor Listy where
  fmap f (Listy x) = fromList $ fmap f x

instance Applicative Listy where
  pure x = fromList [x]
  (<*>) (Listy x) (Listy y) = fromList $ x <*> y

-- | So an alternative
instance Alternative Listy where
  empty = mempty
  (<|>) (Listy []) y = y
  (<|>) x _ = x

  


  





 
