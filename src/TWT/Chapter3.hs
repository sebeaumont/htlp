module TWT.Chapter3 where

-- | A type variable on the lhs of a function type e.g. a -> Int is
-- counted in the negative position, a type variable on the rhs of the
-- function arrow is counted in the positive position: e.g. Int -> a
-- nested functions are counted accordingly and the multiplicative
-- rules for arithmetic are applied to determine if type is covariant
-- (+ve), contravariant (-ve) or invariant (-+). Invariance implies an
-- isomorphism between the lhs and rhs.

-- | + => covariant
newtype T1 a = T1 (Int -> a)

-- | - => contravariant
newtype T2 a = T2 (a -> Int)

-- | - + => invariant
newtype T3 a = T3 (a -> a)

-- | + - = - => contravariant
newtype T4 a = T4 ((Int -> a) -> Int)

-- | - - = + => covariant
newtype T5 a = T5 ((a -> Int) -> Int)

-- | Ony T1 and T5 are (covariant) and thus functors
instance Functor T1 where
  fmap f (T1 a)  = T1 $ fmap f a

instance Functor T5 where
  fmap f (T5 a) = T5 $ \b -> a $ b . f
