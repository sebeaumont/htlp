{-# LANGUAGE TypeFamilies #-}
--{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module TWT.Chapter4
  where

import Data.Typeable ( Typeable, Proxy(Proxy), typeRep )
typeName :: forall a. Typeable a => String
typeName = show . typeRep $ Proxy @a

type family AlwaysUnit a where
  AlwaysUnit a = ()

