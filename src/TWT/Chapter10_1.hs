{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Chapter10_1 - Term level defunctionalisation
module TWT.Chapter10_1
  ( Eval
  , eval
  , ListToMaybe
  , MapList
  , Fst
  ) where

import Prelude hiding (fst)

-- | A type class with a functional dependency to determine the return type
class Eval l r | l -> r where
  eval :: l -> r

-- | fst function 
fst :: (a, b) -> a
fst (a, _) = a

newtype Fst a b = Fst (a, b)

instance Eval (Fst a b) a where
  eval (Fst (a, b)) = a


-- | Exercise-10.1-i - defunctionalise: listToMaybe :: [a] -> Maybe a
listToMaybe :: [a] -> Maybe a
listToMaybe l = case l of
  [] -> Nothing
  (x:_) -> Just x

newtype ListToMaybe a = ListToMaybe [a]

instance Eval (ListToMaybe a) (Maybe a) where
  eval (ListToMaybe []) = Nothing
  eval (ListToMaybe (x:_)) = Just x

-- | Higer order functions can be defunctionalised
-- Note here b is defunctionalised symbol
data MapList b a = MapList (a -> b) [a]

-- | Nota bene: By consing eval (f x) we can propagate evaluation of
-- other defunctionalised symbols, e.g.
-- >>> eval (MapList Fst [("hello", 1), ("world", 2)])
-- ["hello", "world"]
-- >>> eval (MapList ListToMaybe [[1,2,3], [2,3,4], [3,4,5], []])
-- [Just 1,Just 2,Just 3,Nothing]
instance Eval b t => Eval (MapList b a) [t] where
  eval (MapList f []) = []
  eval (MapList f (x:xs)) = eval (f x) : eval (MapList f xs)


