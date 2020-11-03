{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module TWT.Chapter5
  where
import Data.Kind (Constraint, Type)

-- | Heterogenous Lists
-- nota. explicit kind is not strictly necessary as GHC will infer this.
data HList (ts :: [Type]) where
  HNil :: HList '[]                           -- ^ An empty list (of types)
  (:#) :: t -> HList ts -> HList (t ': ts)    -- ^ Cons operator
infixr 5 :#

-- | Total head function for HList
hHead :: HList (t ': ts) -> t
hHead (t :# _) = t

-- | GHC stock deriving doesn't play nice with GADTs
-- so we can define a base case by hand
{-
instance Eq (HList '[]) where
  HNil == HNil = True
-- | and inductively
instance (Eq t, Eq (HList ts)) => Eq (HList (t ': ts)) where
  (a :# as) == (b :# bs) = a == b && as == bs

-- | Exercise 5.3-i implemment Ord for HList

instance Ord (HList '[]) where
  compare HNil HNil = EQ

instance (Ord t, Ord (HList ts)) => Ord (HList (t ': ts)) where
  compare (a :# as) (b :# bs) = compare a b <> compare as bs

-- | Exercise 5.3-ii implement Show for HList

instance Show (HList '[]) where
  show HNil = "HNil"

instance (Show t, Show (HList ts)) => Show (HList (t ': ts)) where
  show (a :# as) = show a <> " :# " <> show as
-}

-- | Type family of constraints allows a fold over any Constraint
type family All (c :: Type -> Constraint) (ts :: [Type]) :: Constraint where
  All c '[] = ()
  All c (t ': ts) = (c t, All c ts)

-- | Such that we can define instances in one place
instance All Eq ts => Eq (HList ts) where
  HNil == HNil = True
  (a :# as) == (b :# bs) = a == b && as == bs

-- | Excercise 5.3-iii re-implement Show and Ord instances in terms of All

instance (All Eq ts, All Ord ts) => Ord (HList ts) where
  compare HNil HNil = EQ
  compare (a :# as) (b :# bs) = compare a b <> compare as bs

instance All Show ts => Show (HList ts) where
  show HNil = "HNil"
  show (a :# as) = show a <> " :# " <> show as
  
