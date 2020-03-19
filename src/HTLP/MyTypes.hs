{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Exercises from Thinking with Types
module HTLP.MyTypes where

import GHC.TypeLits

-- | Examples of closed type families which can be thought of as
-- type level functions (subject to being saturated)

type family Not (p :: Bool) :: Bool where
  Not 'True = 'False
  Not 'False = 'True

type family Or (p :: Bool) (q :: Bool) :: Bool where
  Or 'False _ = 'False
  Or 'True p  = p

-- try and define Functor instances for

newtype T1 a = T1 (Int -> a)

instance Functor T1 where
  fmap :: (a -> b) -> T1 a -> T1 b
  fmap f (T1 a) = T1 $ fmap f a
  

newtype T2 a = T2 (a -> Int)

{-
instance Functor T2 where
  --fmap :: (a -> b) -> T2 a -> T2 b
  fmap f (T2 a) = T2 $ fmap f a
-}

newtype T3 a = T3 (a -> a)

{-
instance Functor T3 where
  -- this is more general than the type signature in the class
  fmap :: (a -> a) -> T3 a -> T3 a
  fmap f (T3 a) = T3 $ fmap f a
-}

newtype T4 a = T4 ((Int -> a) -> Int)

{-
instance Functor T4 where
  fmap f (T4 a) = T4 $ \b -> a (b . f)  
-}

newtype T5 a = T5 ((a -> Int) -> Int)

-- Yay get your head around this...
instance Functor T5 where
  -- follow the types... 
  fmap f (T5 a) = T5 $ \b -> a (b . f)


-- | The trivial or identity monad

newtype W a = W a deriving Show

instance Functor W where
  fmap :: (a -> b) -> W a -> W b
  fmap f (W a) = W (f a)

instance Applicative W where
  pure a = W a
  W f <*> W a = W (f a)

instance Monad W where
  return = pure
  W a >>= f = f a

{-
Monad laws:
return a >>= f == f a                      -- l identity
m >>= return == m                          -- r identity
(m >>= f) >>= g == m >>= (f >>= g)
(m >>= f) >>= g == m >>= (\x -> f x >>= g) -- assoc
-}

-- | Join in terms of bind
join :: W (W a) -> W a
-- join (W a) = a
join m = m  >>= id

bind :: (a -> W b) -> W a -> W b
bind f (W a) = f a


-- | The continutation monad

newtype Cont a = Cont { unCont :: forall r. (a -> r) -> r }

-- | Functor instance for Cont 
instance Functor Cont where
  fmap :: (a -> b) -> Cont a -> Cont b
  -- the new continuation function (b -> r) is composed with the
  -- f'mapped function (a -> b) then the original contiuation (a -> r)
  -- is applied. Cool.
  fmap f (Cont a) = Cont $ \c' -> a $ c' . f

instance Applicative Cont where
  pure :: a -> Cont a
  pure a = Cont $ \f -> f a 
  -- 
  (<*>) :: Cont (a -> b) -> Cont a -> Cont b
  (Cont f) <*> (Cont a)  = Cont $ \b -> f $ \c -> a $ b . c

instance Monad Cont where
  return = pure
  (>>=) :: Cont a -> (a -> Cont b) -> Cont b
  -- look hard at this...
  Cont m >>= f = Cont $ \c -> m $ \a -> unCont (f a) c

  
-- | now the monad transformer for Cont 

newtype ContT m a = ContT { unContT :: forall r. (a -> m r) -> m r }

instance Functor (ContT m) where
  fmap f (ContT a) = ContT $ \c' -> a $ c' . f

instance Applicative (ContT m) where
  pure a = ContT $ \f -> f a
  (<*>) :: ContT m (a -> b) -> ContT m a -> ContT m b
  (ContT f) <*> (ContT a)  = ContT $ \b -> f $ \c -> a $ b . c

instance (Monad m) => Monad (ContT m) where
  return = pure
  (>>=) :: ContT m a -> (a -> ContT m b) -> ContT m b
  ContT m >>= f = ContT $ \c -> m $ \a -> unContT (f a) c

