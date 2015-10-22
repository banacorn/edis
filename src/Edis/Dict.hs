{-# LANGUAGE DataKinds, PolyKinds
    , TypeFamilies, TypeOperators
    , GADTs
    , FlexibleInstances, FlexibleContexts #-} --, KindSignatures, ConstraintKinds #-}

module Edis.Dict where

import GHC.TypeLits
import Data.Proxy

--------------------------------------------------------------------------------
--  Maybe
--------------------------------------------------------------------------------

--  FromJust : (Maybe k) -> k
type family FromJust (x :: Maybe k) :: k where
    FromJust (Just k) = k

--------------------------------------------------------------------------------
--  Dictionary
--------------------------------------------------------------------------------

data Dict :: [ (Symbol, *) ] -> * where
    DNil    :: Dict '[]
    DCons   :: Proxy s  -- key
            -> x        -- value
            -> Dict ts  -- old dict
            -> Dict ('(s, x) ': ts) -- new dict

-- how to print Dict
instance Show (Dict '[]) where
    show DNil = ""
instance (KnownSymbol s, Show x, Show (Dict xs)) => Show (Dict ('(s, x) ': xs)) where
    show (DCons s x xs) = symbolVal s ++ " : " ++ show x ++  "\n" ++ show xs

--------------------------------------------------------------------------------
--  Dictionary Membership
--------------------------------------------------------------------------------

type family Member (s :: Symbol) (xs :: [ (Symbol, *) ]) :: Bool where
    Member s '[]             = False
    Member s ('(s, x) ': xs) = True
    Member s ('(t, x) ': xs) = Member s xs

--------------------------------------------------------------------------------
--  Dictionary Lookup
--------------------------------------------------------------------------------

type family Get (s :: Symbol) (xs :: [ (Symbol, *) ]) :: Maybe * where
    Get s '[]             = Nothing
    Get s ('(s, x) ': xs) = Just x
    Get s ('(t, x) ': xs) = Get s xs

test :: (Get "A" '[ '("A", Int) ] ~ Just Int) => ()
test = ()

--------------------------------------------------------------------------------
--  Dictionary Declaration
--------------------------------------------------------------------------------

-- type family Declare (s :: Symbol) (x :: *) (xs :: [ (Symbol, *) ]) :: [ (Symbol, *) ] where
--     Declare s x '[]             = '[ '(s, x) ]
--     Declare s x ('(t, y) ': xs) = '(t, y) ': (Declare s x xs)

--------------------------------------------------------------------------------
--  Dictionary Set
--------------------------------------------------------------------------------

type family Set (s :: Symbol) (x :: *) (xs :: [ (Symbol, *) ]) :: [ (Symbol, *) ] where
    Set s x '[]             = '[ '(s, x) ]
    Set s x ('(s, y) ': xs) = ('(s, x) ': xs)
    Set s x ('(t, y) ': xs) = '(t, y) ': (Set s x xs)

--------------------------------------------------------------------------------
--  Dictionary Deletion
--------------------------------------------------------------------------------

type family Del (s :: Symbol) (xs :: [ (Symbol, *) ]) :: [ (Symbol, *) ] where
    Del s ('(s, y) ': xs) = xs
    Del s ('(t, y) ': xs) = '(t, y) ': (Del s xs)
