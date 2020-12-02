{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Chapter10_2 - Type level defunctionalisation
module TWT.Chapter10_2 where

import Data.Kind (Constraint, Type)
import Data.Monoid ((<>))

-- | A kind level synonym which descibes a type-level function, which when
-- evaluated, will produce a type of kind a.
type Exp a = a -> Type

-- | The evaluation is performed via an open type family `EvalF`. EvalF matches
-- on `Exp a`s, mapping them to an a.
type family Eval (e :: Exp a) :: a

-- | To write defunctionalised labels, empty data-types can be used
-- e.g. if we wanted to lift `snd` to the type-level, we can write a
-- data type whise kind mirrors the type of `snd`
data Snd :: (a, b) -> Exp b

-- | An instance of EvalF can be used to implement the evaluation of Snd.
type instance Eval (Snd '(a, b)) = b

-- | Functions that perform pattern matching can be lifted to the defunctionalised style
-- by providing multiple type instances for `EvalF`
data FromMaybe :: a -> Maybe a -> Exp a
type instance Eval (FromMaybe _1 ('Just a)) = a
type instance Eval (FromMaybe a 'Nothing) = a

-- | Exercise-10.2-1 - Defunctionalise listToMaybe at the type-level
data ListToMaybe :: [a] -> Exp (Maybe a)
type instance Eval (ListToMaybe '[]) = 'Nothing
type instance Eval (ListToMaybe (x ': _1)) = 'Just x

-- | Higher order functions may be defunctionalised at the type level
data MapList :: (a -> Exp b) -> [a] -> Exp [b]
type instance Eval (MapList f '[]) = '[]
type instance Eval (MapList f (a ': as)) = Eval (f a) ': Eval (MapList f as)

-- | Exercise 10.2-ii - defunctionalise: foldr :: (a -> b -> b) -> b -> [a] -> b
data Foldr :: (a -> b -> Exp b) -> b -> [a] -> Exp b
type instance Eval (Foldr _1 b '[]) = b
type instance Eval (Foldr f b (x ': xs)) = Eval (f x (Eval (Foldr f b xs))) 

-- | First class families form a monad at type level
-- interestingly at the type level (<-<) acts like regular
-- function composition, and (=<<) behaves like function application
data Pure :: a -> Exp a
type instance Eval (Pure x) = x

data (=<<) :: (a -> Exp b) -> Exp a -> Exp b
type instance Eval (k =<< e) = Eval (k (Eval e))
infixr 0 =<<

data (<-<) :: (b -> Exp c) -> (a -> Exp b) -> a -> Exp c
type instance Eval ((f <-< g) x) = Eval (f (Eval (g x)))
infixr 1 <-<

-- | the first-class-families package provides most of the Prelude as
-- FCF's, a well as some useful funcitons for type-level programming:
-- e.g. type equality
data TyEq :: a -> b -> Exp Bool
type instance Eval (TyEq a b) = TyEqImpl a b
type family TyEqImpl (a :: k) (b :: k) :: Bool where
  TyEqImpl a a = 'True
  TyEqImpl a b = 'False

-- ...

-- | 10.4 Ad-hoc polymorphism
data Map :: (a -> Exp b) -> f a -> Exp (f b)
type instance Eval (Map f '[]) = '[]
type instance Eval (Map f (x ': xs)) = Eval (f x) ': Eval (Map f xs)

type instance Eval (Map f 'Nothing) = 'Nothing
type instance Eval (Map f ('Just x)) = 'Just (Eval (f x))
-- etc. giving ad-hoc polymorphism for promoted fmap

-- | Exercise 10.4-1: write a promoted functor instance for tuples
type instance Eval (Map f '(a, b)) = '(a, Eval (f b))


-- | Type level semigroup -- I think we need first-class-families here on
{-
data Mappend :: a -> a -> Exp a
type instance Eval (Mappend '() '()) = '()
type instance Eval (Mappend (a :: Constraint) (b :: Constraint)) = (a, b)
type instance Eval (Mappend (a :: [k]) (b :: [k])) = Eval (a ++ b)
-- Can't find promoted ++?
-}
