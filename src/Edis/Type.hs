{-# LANGUAGE DeriveGeneric, DeriveDataTypeable
    , GADTs, RankNTypes
    , DataKinds, PolyKinds
    , TypeFamilies #-}

module Edis.Type where

import           Edis.Serialize

import           Database.Redis (Reply(..), Redis)
import qualified Database.Redis as Redis

--------------------------------------------------------------------------------
--  P
--------------------------------------------------------------------------------

class PMonad m where
    unit :: a -> m p p a
    bind :: m p q a -> (a -> m q r b) -> m p r b

data P p q a = P { unP :: Redis a }

instance PMonad P where
    unit = P . return
    bind m f = P (unP m >>= unP . f )

infixl 1 >>>

-- Kleisli arrow
(>>>) :: PMonad m => m p q a -> m q r b -> m p r b
a >>> b = bind a (const b)

--------------------------------------------------------------------------------
--  Redis Data Types (and Kinds)
--------------------------------------------------------------------------------

data NoneK = NoneK
data StringK n = StringK n
data HashK n = HashK n
data ListK n = ListK n
data SetK n = SetK n
data ZSetK n = ZSetK n


--------------------------------------------------------------------------------
--  Redis Data Type
--------------------------------------------------------------------------------

type family RType (x :: *) :: Redis.RedisType where
    RType (HashK n) = Redis.Hash
    RType (ListK n) = Redis.List
    RType (SetK n) = Redis.Set
    RType (ZSetK n) = Redis.ZSet
    RType x = Redis.String

type family IsHash (x :: *) :: Bool where
    IsHash (HashK n) = True
    IsHash x         = False

type family IsList (x :: *) :: Bool where
    IsList (ListK n) = True
    IsList x         = False

type family IsSet (x :: *) :: Bool where
    IsSet (SetK n) = True
    IsSet x        = False

type family IsZSet (x :: *) :: Bool where
    IsZSet (ZSetK n) = True
    IsZSet x         = False
