{-# LANGUAGE OverloadedStrings, RankNTypes, DataKinds #-}

module Edis.Lifted where

import Data.Proxy
import GHC.TypeLits
--
-- import Edis.Transaction
import Edis.Serialize
-- import Edis.TypeChecking
import Edis.Type
-- import Data.Typeable

-- data Lifted n a = forall n . KnownSymbol n => Lifted n (Tx a)

-- a :: Set Int "hello"
-- a = Set

-- set :: (forall n . KnownSymbol n, Value a) => a -> Lifted n Status
set v = undefined

-- set :: forall n . KnownSymbol n => Tx Status n
-- set = undefined
--

-- a = set :: Tx Status "haha"
--
-- a :: SomeSymbol
-- a = someSymbolVal "test"
--
-- a' :: SomeSymbol
-- a' = SomeSymbol (Proxy :: Proxy "test")
--
-- foo :: forall n . KnownSymbol n => SomeSymbol -> Int
-- foo (SomeSymbol p) = 3
--


-- foo :: forall n . KnownSymbol n => Proxy n
-- foo :: forall n . KnownSymbol n => SomeSymbol -> Proxy n
-- foo :: forall n . KnownSymbol n => SomeSymbol -> Proxy n
-- foo (SomeSymbol a) = a
