{-# LANGUAGE DeriveGeneric, DeriveDataTypeable
    , GADTs, RankNTypes
    , DataKinds, PolyKinds #-}

module Edis.Type where

import           Edis.Serialize

import           Database.Redis (Reply(..), Redis)

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
--  Kinds
--------------------------------------------------------------------------------

data ListK n = ListK n
data SetK n = SetK n
data HashK n = HashK n
