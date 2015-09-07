{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, GADTs #-}
module Sandbox where

import GHC.TypeLits
import Data.Proxy
import Data.Typeable
import Data.Maybe (isJust)

-- list
data HList :: [*] -> * where
  HNil  :: HList '[]
  HCons :: a -> HList (ts) -> HList (a ': ts)

-- association list
data HAList :: [(*,*)] -> * where
  ANil  :: HAList '[]
  ACons :: a -> HAList ts -> HAList ('(key,a) ': ts)

-- Type Representation
intType :: TypeRep
intType  = typeRep (Proxy :: Proxy Int)
charType :: TypeRep
charType = typeRep (Proxy :: Proxy Char)

-- How to actually construct them from the term level?
foo :: Proxy '("foo", intType)
foo = Proxy

bar :: Proxy '("bar", charType)
bar = Proxy

someAList :: HList '[Proxy '("foo", intType), Proxy '("bar", charType)]
someAList = HCons foo (HCons bar HNil)






insert :: a -> HList ts -> HList (a ': ts)
insert = HCons



-- data Tuple :: (*,*) -> * where
--   Tuple :: a -> b -> Tuple '(a,b)

-- data SList a = SNil | SCons a (SList a)


-- data HList :: SList Symbol -> * where
--     HNil  :: HList SNil
--     HCons :: a -> HList ts -> HList (SCons a ts)

-- data HList :: [Symbol] -> * where
--   HNil  :: HList '[]
--   HCons :: a -> HList (ts) -> HList (a ': ts)

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


-- Symbol :: *
-- KnownSymbol :: Symbol -> Constraint

same :: (KnownSymbol a, KnownSymbol b) => Proxy a -> Proxy b -> Bool
same a b = isJust (sameSymbol a b)
