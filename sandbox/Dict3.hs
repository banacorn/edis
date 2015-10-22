{-# LANGUAGE DataKinds, PolyKinds
    , TypeFamilies, TypeOperators
    , GADTs
    , FlexibleInstances, FlexibleContexts #-} --, KindSignatures, ConstraintKinds #-}

module Dict where

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

-- examples
dict0 :: Dict '[]
dict0 = DNil

dictAB :: Dict '[ '("A", Char), '("B", Int) ]
dictAB = DCons Proxy 'a' (DCons Proxy 0 DNil)

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

-- example
testMemberType0 :: (Member "C" '[ '("A", Char), '("B", Int) ] ~ False) => ()
testMemberType0 = ()

--------------------------------------------------------------------------------
--  Dictionary Lookup
--------------------------------------------------------------------------------

type family Get (s :: Symbol) (xs :: [ (Bool, Symbol, *) ]) :: Maybe * where
    Get s '[]                    = Nothing
    Get s ('(True,  t, x) ': xs) = Just x
    Get s ('(False, t, x) ': xs) = Get s xs

--------------------------------------------------------------------------------
--  Dictionary Declaration
--------------------------------------------------------------------------------

type family Declare (s :: Symbol) (x :: *) (xs :: [ (Symbol, *) ]) :: [ (Symbol, *) ] where
    Declare s x '[]             = '[ '(s, x) ]
    Declare s x ('(t, y) ': xs) = '(t, y) ': (Declare s x xs)

--------------------------------------------------------------------------------
--  Dictionary Update
--------------------------------------------------------------------------------

type family Update (s :: Symbol) (x :: *) (xs :: [ (Symbol, *) ]) :: [ (Symbol, *) ] where
    Update s x ('(s, y) ': xs) = ('(s, x) ': xs)
    Update s x ('(t, y) ': xs) = '(t, y) ': (Update s x xs)

--------------------------------------------------------------------------------
--  Dictionary Deletion
--------------------------------------------------------------------------------

type family Delete (s :: Symbol) (xs :: [ (Symbol, *) ]) :: [ (Symbol, *) ] where
    Delete s ('(s, y) ': xs) = xs
    Delete s ('(t, y) ': xs) = '(t, y) ': (Delete s xs)

--------------------------------------------------------------------------------
--  Parameterized Monad
--------------------------------------------------------------------------------

class PMonad m where
    unit :: a -> m p p a
    bind :: m p q a -> (a -> m q r b) -> m p r b

data M p q a = M { unM :: IO a }

instance PMonad M where
    unit = M . return
    bind m f = M (unM m >>= unM . f )

start :: M '[] '[] ()
start = M (return ())

run :: M p q a -> IO a
run = unM

dec :: (Member s xs ~ False, KnownSymbol s) => Proxy s -> Proxy x -> M xs (Declare s x xs) ()
dec s _ = M $ putStrLn ("Declaring " ++ symbolVal s)

set :: (Member s xs ~ True, KnownSymbol s) => Proxy s -> Proxy x -> M xs (Update s x xs) ()
set s _ = M $ putStrLn ("Updating " ++ symbolVal s)

del :: (Member s xs ~ True, KnownSymbol s) => Proxy s -> M xs (Delete s xs) ()
del s = M $ putStrLn ("Deleting " ++ symbolVal s)

program = start
    `bind` \_ -> dec (Proxy :: Proxy "B") (Proxy :: Proxy Char)
    `bind` \_ -> dec (Proxy :: Proxy "A") (Proxy :: Proxy Char)
    `bind` \_ -> set (Proxy :: Proxy "A") (Proxy :: Proxy Int)
    `bind` \_ -> del (Proxy :: Proxy "A")
