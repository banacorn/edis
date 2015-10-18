{-# LANGUAGE DataKinds, KindSignatures, PolyKinds, ConstraintKinds
            , GADTs, TypeOperators, TypeFamilies
            , MultiParamTypeClasses, FlexibleInstances, OverlappingInstances #-}

module SymList where

import Prelude hiding (lookup)
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
egGet0 :: Get Z '["A", "B"] ~ "A" => ()
egGet0 = ()

-- term level function
get :: Sing n -> SymList xs -> Proxy (Get n xs)
get SZ     (SymCons x xs) = x
get (SS n) (SymCons x xs) = get n xs

-- example of get
egGetTerm0 :: get (Get Z '["A", "B"]) ~ Proxy "A" => ()
egGetTerm0 = ()

--------------------------------------------------------------------------------
--  indexing on SymList
--------------------------------------------------------------------------------

-- type level function
type family Index (s :: Symbol) (xs :: [Symbol]) :: N where
    Index s (s ': xs) = Z
    Index s (x ': xs) = S (Index s xs)

-- example of Index
egIndex0 :: Index "A" '["A", "B"] ~ Z => ()
egIndex0 = ()

egIndex1 :: Index "B" '["A", "B"] ~ (S Z) => ()
egIndex1 = ()

-- term level function
class SIndex s xs where
    index :: Proxy s -> SymList xs -> Sing (Index s xs)

instance SIndex s (s ': xs) where
    index _ _ = SZ

instance (   Index s (x ': xs) ~ S (Index s xs)     -- relation
         ,                         SIndex s xs)
         => SIndex s (x ': xs) where
    index s (SymCons x xs) = SS $ index s xs

--------------------------------------------------------------------------------
--  Searching for a Symbol on a SymList
--------------------------------------------------------------------------------

-- type level function: GET STUCK ON `[] !!
    -- type family Find (s :: k) (xs :: [k]) :: k where
    --     Find s      (s ': xs)   = s               -- found!
    --     Find s      (x ': xs)   = Find s xs       -- keep looking

-- type level function
type family Find (s :: k) (xs :: [k]) :: Maybe k where
    Find s      '[]         = Nothing             -- found!
    Find s      (s ': xs)   = Just s               -- found!
    Find s      (x ': xs)   = Find s xs       -- keep looking

-- example of Find, reified on Proxy
egFind0 :: (Find "A" '["A", "B"]) ~ Just "A" => ()
egFind0 = ()

--------------------------------------------------------------------------------
--  Dictionary
--------------------------------------------------------------------------------

data Dict :: [ (Symbol, *) ] -> * where
    DNil :: Dict '[]
    DCons :: Proxy s    -- key
             -> x       -- value
             -> Dict ts -- old dict
             -> Dict ('(s, x) ': ts) -- new dict

egDict0 :: Dict '[]
egDict0 = DNil

egDict1 :: Dict '[ '("A", Char), '("B", Int) ]
egDict1 = DCons Proxy 'a' (DCons Proxy 0 DNil)

data instance Sing (a :: Maybe k) where
    SNothing :: Sing Nothing
    SJust :: Sing k -> Sing (Just k)

type family FromJust (x :: Maybe k) :: k where
    FromJust (Just k) = k

egFromJust0 :: (FromJust (Just Char) ~ Char) => ()
egFromJust0 = ()

type family Lookup (s :: Symbol) (xs :: [ (Symbol, *) ]) :: Maybe * where
    Lookup s '[]             = Nothing
    Lookup s ('(s, x) ': xs) = Just x
    Lookup s ('(t, x) ': xs) = Lookup s xs

egLookup0 :: (Lookup "A" '[ '("A", Char), '("B", Int) ] ~ Just Char) => ()
egLookup0 = ()

class SLookup s xs where
    lookup :: Proxy s -> Dict xs -> FromJust (Lookup s xs)

instance SLookup s ('(s, x) ': xs) where
    lookup s (DCons _ x _) = x

instance (  (Lookup s ('(t, x) ': xs)) ~ (Lookup s xs)
         ,                               SLookup s xs)
         => SLookup s ('(t, x) ': xs) where
    lookup s (DCons _ _ xs) = lookup s xs


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
