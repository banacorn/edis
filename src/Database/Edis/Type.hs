{-# LANGUAGE DataKinds, ConstraintKinds, PolyKinds,
    TypeFamilies, TypeOperators #-}

module Database.Edis.Type where

import           GHC.TypeLits

import           Data.Type.Bool
import           Data.Type.Equality
import           Database.Redis (Redis)

--------------------------------------------------------------------------------
--  Maybe
--------------------------------------------------------------------------------

--  FromJust : (Maybe k) -> k
type family FromJust (x :: Maybe k) :: k where
    FromJust ('Just k) = k

--------------------------------------------------------------------------------
--  Dictionary Membership
--------------------------------------------------------------------------------

-- Member :: Key -> [ (Key, Type) ] -> Bool
type family Member (xs :: [ (Symbol, *) ]) (s :: Symbol) :: Bool where
    Member '[]             s = 'False
    Member ('(s, x) ': xs) s = 'True
    Member ('(t, x) ': xs) s = Member xs s

-- memberEx0 :: (Member "C" '[] ~ False) => ()
-- memberEx0 = ()
--
-- memberEx1 :: (Member "A" '[ '("A", Char), '("B", Int) ] ~ True) => ()
-- memberEx1 = ()
--
-- memberEx2 :: (Member "C" '[ '("A", Char), '("B", Int) ] ~ False) => ()
-- memberEx2 = ()

--------------------------------------------------------------------------------
--  Dictionary Lookup
--------------------------------------------------------------------------------

-- type family Get' (s :: Symbol) (xs :: [ (Symbol, *) ]) :: * where
--     Get' s ('(s, x) ': xs) = x
--     Get' s ('(t, x) ': xs) = Get' s xs
--
-- getEx0' :: (Get' "A" '[ '("A", Char), '("B", Int) ] ~ Char) => ()
-- getEx0' = ()
--
-- getEx1' :: (Get' "C" '[ '("A", Char), '("B", Int) ] ~ Char) => ()
-- getEx1' = ()

-- Get :: Key -> [ (Key, Type) ] -> Maybe Type
type family Get
    (xs :: [ (Symbol, *) ])
    (s :: Symbol)
    :: Maybe * where

    Get '[]             s = 'Nothing
    Get ('(s, x) ': xs) s = 'Just x
    Get ('(t, x) ': xs) s = Get xs s


-- getEx0 :: (Get "A" '[ '("A", Char), '("B", Int) ] ~ Just Char) => ()
-- getEx0 = ()
--
-- getEx1 :: (Get "C" '[ '("A", Char), '("B", Int) ] ~ Nothing) => ()
-- getEx1 = ()


--------------------------------------------------------------------------------
--  Dictionary Set
--------------------------------------------------------------------------------

-- Set :: Key -> Type -> [ (Key, Type) ] -> [ (Key, Type) ]
type family Set (xs :: [ (Symbol, *) ]) (s :: Symbol) (x :: *) :: [ (Symbol, *) ] where
    Set '[]             s x = '[ '(s, x) ]
    Set ('(s, y) ': xs) s x = ('(s, x) ': xs)
    Set ('(t, y) ': xs) s x = '(t, y) ': (Set xs s x)

-- setEx0 :: (Set "A" Char '[] ~ '[ '("A", Char) ]) => ()
-- setEx0 = ()
--
-- setEx1 :: (Set "A" Bool '[ '("A", Char) ] ~ '[ '("A", Bool) ]) => ()
-- setEx1 = ()

--------------------------------------------------------------------------------
--  Dictionary Deletion
--------------------------------------------------------------------------------

-- Del :: Key -> [ (Key, Type) ] -> [ (Key, Type) ]
type family Del (xs :: [ (Symbol, *) ]) (s :: Symbol) :: [ (Symbol, *) ] where
    Del '[] s             = '[]
    Del ('(s, y) ': xs) s = xs
    Del ('(t, y) ': xs) s = '(t, y) ': (Del xs s)

-- delEx0 :: (Del "A" '[ '("A", Char) ] ~ '[]) => ()
-- delEx0 = ()
--
-- delEx1 :: (Del "A" '[ '("B", Int), '("A", Char) ] ~ '[ '("B", Int) ]) => ()
-- delEx1 = ()

--------------------------------------------------------------------------------
--  P
--------------------------------------------------------------------------------

class IMonad m where
    unit :: a -> m p p a
    bind :: m p q a -> (a -> m q r b) -> m p r b

newtype Edis p q a = Edis { unEdis :: Redis a }

instance IMonad Edis where
    unit = Edis . return
    bind m f = Edis (unEdis m >>= unEdis . f )

infixl 1 >>>

(>>>) :: IMonad m => m p q a -> m q r b -> m p r b
a >>> b = bind a (const b)

--------------------------------------------------------------------------------
--  Redis Data Types
--------------------------------------------------------------------------------

data U n = String' n | Hash' n

data StringOf :: * -> *
data HashOf :: [ (Symbol, *) ] -> *
data ListOf :: * -> *
data SetOf :: * -> *
data ZSetOf :: * -> *

--------------------------------------------------------------------------------
--  Redis Data Type
--------------------------------------------------------------------------------

type family IsString (x :: *) :: Bool where
    IsString (StringOf n) = 'True
    IsString x            = 'False

type StringOrNX xs s          = (IsString (FromJust (Get xs s)) || Not (Member xs s)) ~ 'True
type StringOfIntegerOrNX xs s = ((FromJust (Get xs s) == Integer) || Not (Member xs s)) ~ 'True
type StringOfDoubleOrNX xs s  = ((FromJust (Get xs s) == Double) || Not (Member xs s)) ~ 'True

type family IsHash (x :: *) :: Bool where
    IsHash (HashOf n) = 'True
    IsHash x          = 'False

type HashOrNX xs k = (IsHash (FromJust (Get xs k)) ||  Not (Member xs k)) ~ 'True

type family IsList (x :: *) :: Bool where
    IsList (ListOf n) = 'True
    IsList x          = 'False

type ListOrNX xs s = (IsList (FromJust (Get xs s)) || Not (Member xs s)) ~ 'True

type family IsSet (x :: *) :: Bool where
    IsSet (SetOf n) = 'True
    IsSet x         = 'False

type SetOrNX xs s = (IsSet (FromJust (Get xs s)) || Not (Member xs s)) ~ 'True

type family IsZSet (x :: *) :: Bool where
    IsZSet (ZSetOf n) = 'True
    IsZSet x          = 'False

type family GetHash (xs :: [ (Symbol, *) ]) (k :: Symbol) (f :: Symbol) :: Maybe * where
    GetHash '[]                    k f = 'Nothing
    GetHash ('(k, HashOf hs) ': xs) k f = Get hs f
    GetHash ('(k, x       ) ': xs) k f = 'Nothing
    GetHash ('(l, y       ) ': xs) k f = GetHash xs k f

type family SetHash (xs :: [ (Symbol, *) ]) (k :: Symbol) (f :: Symbol) (x :: *) :: [ (Symbol, *) ] where
    SetHash '[]                    k f x = '(k, HashOf (Set '[] f x)) ': '[]
    SetHash ('(k, HashOf hs) ': xs) k f x = '(k, HashOf (Set hs  f x)) ': xs
    SetHash ('(l, y       ) ': xs) k f x = '(l, y                  ) ': SetHash xs k f x

type family DelHash (xs :: [ (Symbol, *) ]) (k :: Symbol) (f :: Symbol) :: [ (Symbol, *) ] where
    DelHash '[]                    k f = '[]
    DelHash ('(k, HashOf hs) ': xs) k f = '(k, HashOf (Del hs f )) ': xs
    DelHash ('(l, y       ) ': xs) k f = '(l, y                ) ': DelHash xs k f

type family MemHash (xs :: [ (Symbol, *) ]) (k :: Symbol) (f :: Symbol) :: Bool where
    MemHash '[]                    k f = 'False
    MemHash ('(k, HashOf hs) ': xs) k f = Member hs f
    MemHash ('(k, x       ) ': xs) k f = 'False
    MemHash ('(l, y       ) ': xs) k f = MemHash xs k f
