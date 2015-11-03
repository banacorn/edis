{-# LANGUAGE DataKinds, KindSignatures, PolyKinds, ConstraintKinds
            , GADTs, TypeOperators, TypeFamilies
            , MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, OverlappingInstances #-}

module SymList where

import Prelude hiding (lookup)
import GHC.TypeLits
import Data.Proxy
import GHC.Exts (Constraint)
-- https://jeltsch.wordpress.com/2013/02/14/the-constraint-kind/
-- https://www.cs.ox.ac.uk/projects/utgp/school/andres.pdf
-- https://typesandkinds.wordpress.com/
-- http://dev.stephendiehl.com/hask/#higher-kinds
-- http://research.microsoft.com/en-us/um/people/simonpj/papers/assoc-types/fun-with-type-funs/typefun.pdf
-- http://bentnib.org/paramnotions-jfp.pdf
--------------------------------------------------------------------------------
--  data family Sing (a :: k)
--------------------------------------------------------------------------------

data family Sing (a :: k)
type Sym = (Proxy :: Symbol -> *)

--------------------------------------------------------------------------------
--  Maybe
--------------------------------------------------------------------------------

-- singleton type of Maybe
data instance Sing (a :: Maybe k) where
    SNothing :: Sing Nothing
    SJust :: Sing k -> Sing (Just k)

type family FromJust (x :: Maybe k) :: k where
    FromJust (Just k) = k

fromJust0 :: (FromJust (Just Char) ~ Char) => ()
fromJust0 = ()

--------------------------------------------------------------------------------
--  Dictionary
--------------------------------------------------------------------------------

data Dict :: [ (Symbol, *) ] -> * where
    DNil :: Dict '[]
    DCons :: Proxy s    -- key
             -> x       -- value
             -> Dict ts -- old dict
             -> Dict ('(s, x) ': ts) -- new dict

dict0 :: Dict '[]
dict0 = DNil

dictAB :: Dict '[ '("A", Char), '("B", Int) ]
dictAB = DCons Proxy 'a' (DCons Proxy 0 DNil)

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

member0 :: (Member "C" '[ '("A", Char), '("B", Int) ] ~ False) => ()
member0 = ()

--------------------------------------------------------------------------------
--  Dictionary Lookup
--------------------------------------------------------------------------------

-- wontWork :: Sym s -> Dict xs -> FromJust (Get s xs)
-- wontWork s DNil = error "empty dict"
-- wontWork s (DCons t x xs) | symbolVal s == symbolVal t = x
--                           | symbolVal s /= symbolVal t = wontWork s xs

type family Get (s :: Symbol) (xs :: [ (Symbol, *) ]) :: Maybe * where
    Get s '[]             = Nothing
    Get s ('(s, x) ': xs) = Just x
    Get s ('(t, x) ': xs) = Get s xs

get0 :: (Get "A" '[ '("A", Char), '("B", Int) ] ~ Just Char) => ()
get0 = ()

class SGet s xs where
    get :: Sym s -> Dict xs -> FromJust (Get s xs)

instance SGet s ('(s, x) ': xs) where
    get s (DCons _ x _) = x

instance (  (Get s ('(t, x) ': xs)) ~ (Get s xs)
         ,                            SGet s xs)
         => SGet s ('(t, x) ': xs) where
    get s (DCons _ _ xs) = get s xs

sget0 :: FromJust (Get "A" '[ '("A", Char), '("B", Int) ])
sget0 = get (Proxy :: Proxy "A") dictAB

--------------------------------------------------------------------------------
--  Dictionary Insertion/Update
--------------------------------------------------------------------------------

type family Set (s :: Symbol) (x :: *) (xs :: [ (Symbol, *) ]) :: [ (Symbol, *) ] where
    Set s x '[]             = '[ '(s, x) ]
    Set s x ('(s, y) ': xs) = ('(s, x) ': xs)
    Set s x ('(t, y) ': xs) = '(t, y) ': (Set s x xs)

set0 :: (Set "A" Char '[] ~ '[ '( "A", Char ) ]) => ()
set0 = ()

set1 :: (Set "A" Char '[ '("B", Int) ] ~ '[ '("B", Int), '("A", Char) ]) => ()
set1 = ()

set2 :: (Set "A" Char '[ '("A", Int) ] ~ '[ '("A", Char) ]) => ()
set2 = ()

class SSet s x xs where
    set :: Sym s -> x -> Dict xs -> Dict (Set s x xs)

instance SSet s x '[] where
    set s x _ = DCons s x DNil

instance SSet s x ('(s, y) ': xs) where
    set s x (DCons _ _ xs) = DCons s x xs

instance (   Set s x ('(t, y) ': xs) ~ ('(t, y) ': (Set s x xs))
         ,                                         SSet s x xs )
         => SSet s x ('(t, y) ': xs) where
    set s x (DCons t y xs) = DCons t y (set s x xs)

sset0 :: Dict (Set "A" Bool '[ '("A", Char), '("B", Int) ])
sset0 = set (Proxy :: Proxy "A") True dictAB

--------------------------------------------------------------------------------
--  Dictionary Deletion
--------------------------------------------------------------------------------

type family Del (s :: Symbol) (xs :: [ (Symbol, *) ]) :: [ (Symbol, *) ] where
    Del s '[]             = '[]
    Del s ('(s, y) ': xs) = xs
    Del s ('(t, y) ': xs) = '(t, y) ': (Del s xs)

del0 :: (Del "A" '[] ~ '[]) => ()
del0 = ()

del1 :: (Del "A" '[ '("A", Char) ] ~ '[]) => ()
del1 = ()

del2 :: (Del "A" '[ '("B", Int) ] ~ '[ '("B", Int) ]) => ()
del2 = ()

del3 :: (Del "A" '[ '("B", Int), '("A", Char) ] ~ '[ '("B", Int) ]) => ()
del3 = ()

class SDel s xs where
    del :: Sym s -> Dict xs -> Dict (Del s xs)

instance SDel s '[] where
    del _ _ = DNil

instance SDel s ('(s, x) ': xs) where
    del s (DCons _ x xs) = xs

instance (   Del s ('(t, x) ': xs) ~ ('(t, x) ': Del s xs)
         ,                                      SDel s xs)
         => SDel s ('(t, x) ': xs) where
    del s (DCons t x xs) = DCons t x (del s xs)

sdel0 :: Dict (Del "A" '[ '("A", Char), '("B", Int) ])
sdel0 = DCons (Proxy :: Proxy "B") 3 DNil

--------------------------------------------------------------------------------
--  Parameterized Deletion
--------------------------------------------------------------------------------

class PMonad m where
    unit :: a -> m p p a
    bind :: m p q a -> (a -> m q r b) -> m p r b

data TestM p q a = TestM { unTestM :: a }

instance PMonad TestM where
    unit = TestM
    bind m f = TestM (unTestM (f (unTestM m)))

data IOM p q a = IOM { unIOM :: IO a }

instance PMonad IOM where
    unit = IOM . return
    bind m f = IOM (unIOM m >>= unIOM . f )

putM :: String -> IOM p p ()
putM = IOM . putStrLn

setM :: (KnownSymbol s) => Proxy s -> x -> IOM xs (Set s x xs) ()
setM s x = IOM $ putStrLn ("Setting " ++ symbolVal s)

delM :: (KnownSymbol s) => Proxy s -> IOM xs (Del s xs) ()
delM s = IOM $ putStrLn ("Deleting " ++ symbolVal s)

start :: IOM '[] '[] ()
start = IOM (return ())

run :: IOM p q a -> IO a
run = unIOM


-- program =   start   `bind` \_ ->
--             setM (Proxy :: Proxy "B") 'a' `bind` \_ ->
--             setM (Proxy :: Proxy "A") True `bind` \_ ->
--             putM "hey" `bind` \_ ->
--             delM (Proxy :: Proxy "A") `bind` \_ ->
--             unit ()
