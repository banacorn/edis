{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, GADTs #-}
module Sandbox where

import GHC.TypeLits
import Data.Proxy
import Data.Maybe (isJust)


-- Symbol :: *
-- KnownSymbol :: Symbol -> Constraint



data SList a = SNil | SCons a (SList a)



-- data HList :: SList Symbol -> * where
--     HNil  :: HList SNil
--     HCons :: a -> HList ts -> HList (SCons a ts)

-- data HList :: [Symbol] -> * where
--   HNil  :: HList '[]
--   HCons :: a -> HList (ts) -> HList (a ': ts)

data HList :: [*] -> * where
  HNil  :: HList '[]
  HCons :: a -> HList (ts) -> HList (a ': ts)



-- a :: KnownSymbol a => HList '[a]
-- a = HCons _ HNil

-- data HList :: SList -> * where
--   HNil  :: HList SNil
--   HCons :: Symbol -> HList ts -> HList (SCons a ts)

--
-- three :: HList (SCons "haha" SNil)
-- three = HCons _ HNil
-- append :: Vec a n -> Vec a m -> Vec a (n :+: m)
-- append Nil         bs = bs
-- append (Cons a as) bs = Cons a (append as bs)


foo :: Proxy "foo"
foo = Proxy
bar :: Proxy "bar"
bar = Proxy

a :: HList '[Proxy "foo", Proxy "bar"]
a = HCons foo (HCons bar HNil)

same :: (KnownSymbol a, KnownSymbol b) => Proxy a -> Proxy b -> Bool
same a b = isJust (sameSymbol a b)
