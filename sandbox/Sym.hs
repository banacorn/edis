{-# LANGUAGE DataKinds, KindSignatures, PolyKinds, ConstraintKinds
            , GADTs, TypeOperators, TypeFamilies
            , MultiParamTypeClasses, FlexibleInstances, OverlappingInstances #-}

module SymList where

import GHC.TypeLits
import Data.Proxy

--------------------------------------------------------------------------------
--  kind-indexed singletons
--
--  Sing   
--
--------------------------------------------------------------------------------
data family Sing (a :: k)

--------------------------------------------------------------------------------
--  Natural number à la peano, promoted
--
--  Value-level  Type-level         Models
--  -----------  ------------       -------
--  SZ           Sing 'Z            0
--  SS SZ        Sing ('S 'Z)       1
--  SS (SS SZ)   Sing ('S ('S 'Z))  2
--
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
--  Maybe
--------------------------------------------------------------------------------

data instance Sing (a :: Maybe k) where
    SNothing :: Sing Nothing
    SJust :: Sing k -> Sing (Just k)

type family FromJust (x :: Maybe k) :: k where
    FromJust (Just k) = k

egFromJust0 :: (FromJust (Just Char) ~ Char) => ()
egFromJust0 = ()
