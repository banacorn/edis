{-# LANGUAGE DataKinds, KindSignatures, GADTs, TypeOperators, PolyKinds, TypeFamilies, MultiParamTypeClasses, FlexibleInstances, OverlappingInstances #-}

module Sandbox2 where

import GHC.TypeLits
import Data.Proxy

--------------------------------------------------------------------------------
--  Heterogeneous list
--------------------------------------------------------------------------------

data HList :: [ * ] -> * where
    HNil :: HList '[]
    HCons :: a -> HList ts -> HList (a ': ts)

-- example
h0 :: HList '[Int,     Bool,       (),      Char]
h0 =  HCons   3 (HCons True (HCons () (HCons 'a' HNil)))



--------------------------------------------------------------------------------
--  Nat (type level)
--------------------------------------------------------------------------------
--  type term   term
data N = Zero | Suc N

-- example
n3 :: N
n3 = Suc (Suc (Suc Zero))



--------------------------------------------------------------------------------
--  Nat (term level), indexed by type level Nat
--------------------------------------------------------------------------------
--   type   kind -> kind
data SNat :: N -> * where
    SZero :: SNat 'Zero
    SSuc  :: SNat n -> SNat ('Suc n)

-- example
sn3 :: SNat (Suc (Suc (Suc Zero)))
sn3 = SSuc (SSuc (SSuc SZero))



--------------------------------------------------------------------------------
--  Nth (type level & term level)
--------------------------------------------------------------------------------

type family Nth (n :: N) (xs :: [k]) :: k where
    Nth 'Zero          (x ': xs)   = x
    Nth ('Suc n)       (x ': xs)   = Nth n xs

-- nth : forall xs . (n : Nat) -> (HList xs) -> xs !! n
nth :: SNat n -> HList xs -> Nth n xs
nth SZero    (HCons x xs) = x
nth (SSuc n) (HCons x xs) = nth n xs

-- example
nth0 :: Nth 'Zero '[Int, Bool]
nth0 = 2

nth1 :: Char
nth1 = nth sn3 h0 -- => 'a'


--------------------------------------------------------------------------------
--  Symbol list
--------------------------------------------------------------------------------
-- Symbol :: Kind
-- "asdf", "asf", "asdfsf" :: Symbol

--   type
data SymList :: [ Symbol ] -> * where
    SymNil :: SymList '[]
    SymCons :: Proxy s -> SymList ts -> SymList (s ': ts)

-- data Proxy a = Proxy


-- example
symList0 :: SymList '[]
symList0 = SymNil

symList1 :: SymList '[ "haha" ,      "hehe",        "lol" ]
symList1 = SymCons     Proxy (SymCons Proxy (SymCons Proxy SymNil))



--------------------------------------------------------------------------------
--  Find on Symbol List (type level & term level)
--------------------------------------------------------------------------------

type family SFind (s :: Symbol) (xs :: [Symbol]) :: Symbol where
    SFind s      (s ': xs)   = s                -- found!
    SFind s      (x ': xs)   = SFind s xs       -- keep looking
    -- SFind s      '[]         = "not found"      -- ???

sfind :: Proxy s -> SymList xs -> Proxy (SFind s xs)
sfind s xs = Proxy

-- example
-- sfind0 :: Proxy "hehe"
sfind0 :: Proxy (SFind "hehe" '[ "haha" , "hehe", "lol" ])
sfind0 = Proxy -- lame

-- symbolVal :: Proxy s -> String

sfind1 :: String
sfind1 = symbolVal (sfind (Proxy :: Proxy "hehe") symList1)
-- sfind1 => "haha"

--------------------------------------------------------------------------------
--  Associtate List
--------------------------------------------------------------------------------

data Dict :: [ (Symbol, *) ] -> * where
    DictNil :: Dict '[]
    DictCons :: Proxy s -- key
             -> x       -- value
             -> Dict ts -- old dict
             -> Dict ('(s, x) ': ts) -- new dict

-- example
dict0 :: Dict '[ '("hahahahah", Char), '("int", Int) ]
dict0 = DictCons (Proxy :: Proxy "hahahahah") 'a' (DictCons (Proxy :: Proxy "int") 3 DictNil)

-- instance Show (Dict '[]) where
--     show DictNil = "[]"
-- instance Show (Dict (a ': xs)) where
--     show (DictCons k v xs) = show xs

    -- show (DictCons key val xs) = _
--------------------------------------------------------------------------------
--  Lookup on Dict (type level & term level)
--------------------------------------------------------------------------------

type family Lookup (s :: Symbol) (xs :: [(Symbol, *)]) :: * where
    Lookup s ('(s, x) ': xs) = x              -- found!
    Lookup s ('(z, x) ': xs) = Lookup s xs    -- keep searching

-- class Findable (s :: Symbol) (xs :: [(Symbol, *)])  where
--     find :: Proxy s -> Dict xs -> Find s xs
--
-- instance Findable s ('(s, x) ': xs) where
--     find s (DictCons s' x xs) = x
--
-- instance (Findable s xs) => Findable s ('(z, x) ': xs) where
--     find s (DictCons s' x xs) = find s xs
--
-- -- find s DictNil            = undefined
-- find s (DictCons s' x xs) = undefined
-- find s (DictCons s' x xs) | s == s'   = x
--                           | s /= s'   = find s xs
--
-- -- example
-- find0 :: Lookup "bool" '[ '("bool", Bool), '("int", Int) ]
-- find0 = True



-- --------------------------------------------------------------------------------
-- --  Parameterized Monad
-- --------------------------------------------------------------------------------
--
-- class PMonad m where
--     unit  :: a -> m p p a
--     bind :: m p q a -> (a -> m q r b) -> m p r b
--
-- newtype VST m p q a = VST { unVST :: p -> m (a, q) }
--
-- instance Monad m => PMonad (VST m) where
--     unit x = VST (\s -> return (x, s))
--     (VST m) `bind` f = VST (\s0 -> do
--           (a, s1) <- m s0
--           unVST (f a) s1)
--
-- data TypeRep = TypeRep String
--     deriving (Eq, Show)
--
-- -- insert a key-type relation
-- insert :: Monad m => Proxy s -> TypeRep -> VST m (Dict ts) (Dict ('(s, TypeRep) ': ts)) ()
-- insert key typ = VST $ \dict -> return ((), DictCons key typ dict)
--
-- get :: Monad m => Proxy s -> VST m (Dict ts) (Dict ts) (Find s ts)
-- get key = VST $ \dict -> return (find key dict, dict)
