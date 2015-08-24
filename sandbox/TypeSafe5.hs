{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeOperators,
      TypeFamilies, PolyKinds, UndecidableInstances #-}

module TypeSafe5 where

import GHC.TypeLits
import Data.Proxy
import Data.Type.Equality

data HList :: [*] -> * where
  HNil  :: HList '[]
  HCons :: a -> HList ts -> HList (a ': ts)

type family If (p :: Bool) (a :: k) (b :: k) :: k where
  If 'True a b = a
  If 'False a b = b

type family Find (k :: a) (ls :: [(a,b)]) :: b
-- type instance Lookup k '[] = 'Nothing
type instance Find k ('(a,b) ': xs) = If (a == k) b (Find k xs)

data HAList :: [(Symbol,*)] -> * where
  ANil  :: HAList '[]
  ACons :: a -> HAList ts -> HAList ('(key,a) ': ts)

{-
what about
 ACons :: Proxy key -> a -> HAList ts -> HAList ('(key,a) ': ts)
-}

prox :: Proxy "here"
prox = Proxy

symb :: String
symb = symbolVal prox

{-
find :: Proxy key -> HAList ts -> Find key ts
find Proxy ANil = error "impossibe??"
find Proxy (ACons x xs) = ??
-}
