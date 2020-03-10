> {-# LANGUAGE GADTs, DataKinds, TypeFamilies, TypeOperators, KindSignatures, RankNTypes, PolyKinds #-}
> {-# LANGUAGE TemplateHaskell, ScopedTypeVariables, UndecidableInstances, TypeApplications #-}
> -- {-# LANGUAGE NoImplicitPrelude #-}
> module HTLP.Singletons where

We start working with the `singletons` library
--------------------------------------------------

[Which is here](https://hackage.haskell.org/package/singletons)
and can be installed using e.g. `cabal`.

> import HTLP.Intro
> -- import Data.Singletons
> import Data.Kind
> -- import Data.Singletons.Prelude
> -- import Data.Singletons.Prelude.Enum
> -- import Data.Singletons.TH


Safe head function that specifies that it can only be called on
non-empty lists, since the type index to Vec is the sucessor to some
number n the pattern `Vec a ('Succ n) ~ Vec a 'Zero` can never hold so
the case is impossible and GHC would issue a compile time error should
this case be added.

> head :: Vec a ('Succ n) -> a
> head (VCons h _) = h

In the previous section in the `nth` example `:<` is a type level
function and can be used in a constraint so that imdexing a vector is
only valid when the index is in range.

The singletons library supports the automatic resue of runtime
functions at the type-level. The following `Template Haskell` splice
defines `plus` as a normal function

    $(promote [d|
      plus :: Nat -> Nat -> Nat
      plus Zero m = m
      plus (Succ n) m = Succ (plus n m) |])

NOTA I'm doing this my hand here beacuse it appears not to work as expected.
Could be beacuse of definitions in Data.Singletons.Prelude.Enum which
are necessary for the splice above. I don't like TH anyway really. Another
orthogonal syntax. Great argument for lisp macros and this `dependent type`
fakery is a great argument for `Idris` so far.

> plus :: Nat -> Nat -> Nat
> plus Zero m = m
> plus (Succ n) m = Succ (plus n m)

and also generates the new definition:

> type family Plus (n :: Nat) (m :: Nat) :: Nat
> type instance Plus 'Zero m = m
> type instance Plus ('Succ n) m = 'Succ (Plus n m)

We can now define append in terms of `plus` or rather the promoted
type level function `Plus`

> append :: Vec a n -> Vec a m -> Vec a (Plus n m)
> append VNil v = v
> append (VCons h t) v = VCons h (append t v)

So far we have not made any use of the `singletons` library! Lets hope
we cut to the chase soon... about to encounter data families.

This was the original way Sing was defined: 

   data family Sing (a :: k)

but since singletons 2.6 it is now a `type family` and these produce
exacly the error I was tracking down in grenade and caused me to take
on this singletons stuff in the first place! So what's the story? What
changed in singletons to break this. Maybe be we should be using TH to
make things more maintainable...

   data instance Sing (a :: Bool) where
     STrue :: Sing 'True
     SFalse :: Sing 'False

   data instance Sing (a :: Nat) where
     SZero :: Sing 'Zero

[Here's the answer singletons 2.6](https://github.com/goldfirere/singletons/blob/master/CHANGES.md)

So now `Sing` is defined as [a type family](https://github.com/goldfirere/singletons/blob/27245e24d588a63620ffb7f0276345ac36f8a6ba/src/Data/Singletons/Internal.hs#L52)  

> type family Sing :: k -> Type

> data SBool :: Bool -> Type where
>  SFalse :: SBool False
>  STrue  :: SBool True

> type instance Sing @Bool = SBool

> type instance Sing @Nat = SNat


Also SingRep was removed from singletons at 0.9.0!

   SSucc :: SingKind n -> Sing n -> Sing ('Succ n)

