{-# LANGUAGE DataKinds, KindSignatures, PolyKinds, ConstraintKinds
            , GADTs, TypeOperators, TypeFamilies
            , MultiParamTypeClasses, FlexibleInstances, OverlappingInstances #-}

module SymList where

import GHC.TypeLits
import Data.Proxy
import GHC.Exts (Constraint)
--------------------------------------------------------------------------------
--  kind-indexed singleton
--------------------------------------------------------------------------------

data family Sing (a :: k)

--------------------------------------------------------------------------------
--  Natural Number
--------------------------------------------------------------------------------

data N = Z | S N

data instance Sing (a :: N) where
    SZ :: Sing Z
    SS :: Sing n -> Sing (S n)

toInt :: Sing (a :: N) -> Int
toInt SZ = 0
toInt (SS n) = 1 + toInt n

instance Show (Sing (a :: N)) where
    show n = show (toInt n)

egSN0 :: Sing Z
egSN0 = SZ

egSN1 :: Sing (S (S Z))
egSN1 = SS (SS SZ)

--------------------------------------------------------------------------------
--  Symbol List
--------------------------------------------------------------------------------

data SymList :: [ Symbol ] -> * where
    SymNil :: SymList '[]
    SymCons :: Proxy s -> SymList ts -> SymList (s ': ts)

-- example of SymList
egSymList0 :: SymList '[]
egSymList0 = SymNil

egSymList1 :: SymList '["A", "B"]
egSymList1 = SymCons (Proxy :: Proxy "A") (SymCons (Proxy :: Proxy "B") SymNil)

--------------------------------------------------------------------------------
--  (!!) on SymList
--------------------------------------------------------------------------------

-- type level function
type family Get (n :: N) (xs :: [Symbol]) :: Symbol where
    Get Z     (x ': xs) = x
    Get (S n) (x ': xs) = Get n xs

-- example of Get
egGet0 :: Get Z '["A", "B"] ~ "A" => Proxy a
egGet0 = Proxy

-- term level function
get :: Sing n -> SymList xs -> Proxy (Get n xs)
get SZ     (SymCons x xs) = x
get (SS n) (SymCons x xs) = get n xs

-- example of get
egGetTerm0 :: get (Get Z '["A", "B"]) ~ Proxy "A" => Proxy a
egGetTerm0 = Proxy

--------------------------------------------------------------------------------
--  elem on SymList
--------------------------------------------------------------------------------

-- type level function
type family Elem (s :: Symbol) (xs :: [Symbol]) :: Constraint where
    Elem s (s ': xs) = (() :: Constraint)
    Elem s (x ': xs) = Elem s xs

egElem0 :: Elem "C" '["A", "B"] ~ (() :: Constraint) => Proxy a
egElem0 = Proxy

--------------------------------------------------------------------------------
--  indexing on SymList
--------------------------------------------------------------------------------

-- type level function
type family Index (s :: Symbol) (xs :: [Symbol]) :: N where
    Index s (s ': xs) = Z
    Index s (x ': xs) = S (Index s xs)

-- example of Index
egIndex0 :: Index "A" '["A", "B"] ~ Z => Proxy a
egIndex0 = Proxy

egIndex1 :: Index "B" '["A", "B"] ~ (S Z) => Proxy a
egIndex1 = Proxy

-- term level function

class I s xs where
    index :: Proxy s -> SymList xs -> Sing (Index s xs)

instance I s '[] where
    index = error "empty"

instance I s (s ': xs) where
    index _ _ = SZ

instance (I s xs , Index s (x ': xs) ~ S (Index s xs)) => I s (x ': xs) where
    index s (SymCons x xs) = SS $ index s xs

--------------------------------------------------------------------------------
--  Searching for a Symbol on a SymList
--------------------------------------------------------------------------------

-- type level function
type family Find (s :: Symbol) (xs :: [Symbol]) :: Symbol where
    Find s      (s ': xs)   = s                -- found!
    Find s      (x ': xs)   = Find s xs       -- keep looking

-- example of Find, reified on Proxy
egFind0 :: (Find "A" '["A", "B"]) ~ "A" => Proxy a
egFind0 = Proxy
--
-- type family Elem (s :: Symbol) (xs :: [Symbol]) :: Bool where
--     Elem s (s ': xs) = True --(() :: Constraint)
--     Elem s (x ': xs) = Elem s xs
--
-- egHere0 :: (Elem "C" '["A", "B"] ~ True) => Proxy a
-- egHere0 = Proxy

-- data Member = Here | There
-- find :: (KnownSymbol s) => Proxy s -> String
-- find s = symbolVal s

-- find :: (KnownSymbol s, KnownSymbol x) => Proxy s -> SymList (x ': xs) -> String
-- find s (SymCons x xs) = case sameSymbol s x of
--     Just refl -> symbolVal s
--     Nothing   -> find s xs
-- find s (SymCons x xs) = case sameSymbol s x of
--     Just refl -> symbolVal s
--     Nothing   -> find s xs


-- find :: (s ~ Find s (x ': xs)) => Proxy s -> SymList (x ': xs) -> Proxy (Find s (x ': xs))
-- find s (SymCons x xs) =  case sameSymbol s x of
--     Just refl -> s
--     Nothing   -> find s xs




-- term level function, DOESNT COMPILE!!!!
-- find :: Proxy s -> SymList xs -> Proxy (Find s xs)
-- find s SymNil         = error "empty symbol list"
-- find s (SymCons x xs) = case sameSymbol s x of
--     Just refl -> s
--     Nothing   -> undefined
