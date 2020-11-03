{-# LANGUAGE RankNTypes #-}
module TWT.Chapter6
  ( Cont
  , cont
  , runCont
  , unCont
  , ContT
  , unContT
  , contT
  ) where

-- | Continuation type
newtype Cont a = Cont
  { unCont :: forall r. (a -> r) -> r
  }

cont :: a -> Cont a
cont a = Cont $ \f -> f a

runCont :: Cont a -> a
runCont (Cont c) = c id

-- | Apply the continuation `c` to the new continuation `c'` composed with the
-- mapped over function `f`

instance Functor Cont where
  fmap f (Cont a) =
    Cont $ \g -> a $ g . f

instance Applicative Cont where
  pure = cont
  -- a :: ((a -> b) -> r) -> r
  -- b :: ((a -> r) -> r
  -- g :: b -> r
  -- h :: a -> b
  (Cont a) <*> (Cont b) =
    Cont $ \g -> a $ \h -> b $ g . h

instance Monad Cont where
  -- m :: (a -> r) -> r
  -- f :: a -> Cont b
  -- g :: b -> r
  -- a :: a
  -- b :: (b -> r) -> r
  (Cont m) >>= f =
    Cont $ \g -> m $ \a -> let (Cont b) = f a in b g


-- | Continuation monad transformer version

newtype ContT m a = ContT
  { unContT :: forall r. (a -> m r) -> m r
  }

contT :: a -> ContT m a
contT a = ContT $ \f -> f a

instance Functor (ContT m) where
  fmap f (ContT a) =
    ContT $ \g -> a $ g . f

instance Applicative (ContT m) where
  pure = contT
  (ContT a) <*> (ContT b) =
    ContT $ \g -> a $ \h -> b $ g . h

instance Monad (ContT m) where
  (ContT m) >>= f =
    ContT $ \g -> m $ \a -> unContT (f a) g

