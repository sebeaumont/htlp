{-# LANGUAGE InstanceSigs #-}

-- | Playing around with good stuff and  exercises in type foo.

module HTLP.Play where


-- | The trivial monad
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

