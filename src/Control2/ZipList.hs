{-# LANGUAGE InstanceSigs #-}

module Control2 where

class Applicative2 f where
  purely :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

newtype ZipList a = ZipList { getZipList :: [a] } deriving (Show)
  
instance Applicative2 ZipList where
  purely :: a -> ZipList a
  purely x = ZipList [x] 

  (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
  (ZipList gs) <*> (ZipList xs) = ZipList (zipWith ($) gs xs)

