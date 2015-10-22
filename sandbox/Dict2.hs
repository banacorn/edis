{-# LANGUAGE DataKinds, KindSignatures, PolyKinds, ConstraintKinds
            , GADTs, TypeOperators, TypeFamilies, FlexibleContexts
            , MultiParamTypeClasses, FlexibleInstances, OverlappingInstances #-}

module SymList where

-- import Data.Type.Equality
import Prelude hiding (lookup)
import GHC.TypeLits
import Data.Proxy
import Data.Typeable
import GHC.Exts (Constraint)
-- https://jeltsch.wordpress.com/2013/02/14/the-constraint-kind/
-- https://www.cs.ox.ac.uk/projects/utgp/school/andres.pdf
-- https://typesandkinds.wordpress.com/
-- http://dev.stephendiehl.com/hask/#higher-kinds
-- http://research.microsoft.com/en-us/um/people/simonpj/papers/assoc-types/fun-with-type-funs/typefun.pdf
-- http://bentnib.org/paramnotions-jfp.pdf
-- http://okmij.org/ftp/Haskell/typeEQ.html#anti-over
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

-- singleton type of Bool
data instance Sing (a :: Bool) where
    STrue :: Sing True
    SFalse :: Sing False

type family FromJust (x :: Maybe k) :: k where
    FromJust (Just k) = k

testFromJust0 :: (FromJust (Just Char) ~ Char) => ()
testFromJust0 = ()

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

testMemberType0 :: (Member "C" '[ '("A", Char), '("B", Int) ] ~ False) => ()
testMemberType0 = ()

--------------------------------------------------------------------------------
--  WUT
--------------------------------------------------------------------------------

type family TagEq (s :: Symbol) (xs :: [ (Symbol, *) ]) :: [ (Bool, Symbol, *) ] where
    TagEq s '[]             = '[]
    TagEq s ('(s, x) ': xs) = '(True, s, x) ': TagEq s xs
    TagEq s ('(t, x) ': xs) = '(False, t, x) ': TagEq s xs

testTagEq0 :: (TagEq "A" '[] ~ '[]) => ()
testTagEq0 = ()

testTagEq1 :: (TagEq "A" '[ '("A", Char) ] ~ '[ '(True, "A", Char) ]) => ()
testTagEq1 = ()

testTagEq2 :: (TagEq "A" '[ '("B", Char) ] ~ '[ '(False, "B", Char) ]) => ()
testTagEq2 = ()

type family RipEq (xs :: [ (Bool, Symbol, *) ]) :: [ (Symbol, *) ] where
    RipEq '[]                = '[]
    RipEq ('(t, s, x) ': xs) = '(s, x) ': RipEq xs

testRipEq0 :: (RipEq '[] ~ '[]) => ()
testRipEq0 = ()

testRipEq1 :: (RipEq '[ '(True, "A", Char) ] ~ '[ '("A", Char) ]) => ()
testRipEq1 = ()

--------------------------------------------------------------------------------
--  Tagged Dictionary
--------------------------------------------------------------------------------

data TaggedDict :: [ (Bool, Symbol, *) ] -> * where
    TDNil :: TaggedDict '[]
    TDCons  :: Sing b   -- tag
            -> Proxy s  -- key
            -> x        -- value
            -> TaggedDict ts -- old dict
            -> TaggedDict ( '(b, s, x) ': ts) -- new dict

taggedDict0 :: TaggedDict '[]
taggedDict0 = TDNil

taggedDictAB :: TaggedDict '[ '(False, "A", Char), '(False, "B", Int) ]
taggedDictAB = TDCons SFalse Proxy 'a' (TDCons SFalse Proxy 0 TDNil)

--------------------------------------------------------------------------------
--  Dictionary Lookup
--------------------------------------------------------------------------------

type family Get (s :: Symbol) (xs :: [ (Bool, Symbol, *) ]) :: Maybe * where
    Get s '[]                    = Nothing
    Get s ('(True,  t, x) ': xs) = Just x
    Get s ('(False, t, x) ': xs) = Get s xs

testGetType0 :: (Get "A" (TagEq "A" '[ '("A", Char), '("B", Int) ]) ~ Just Char) => ()
testGetType0 = ()

testGetType1 :: (Get "C" (TagEq "C" '[ '("A", Char), '("B", Int) ]) ~ Nothing) => ()
testGetType1 = ()

-- given a symbol and a tagged list
class Accessible s xs where
    get :: Sym s -> TaggedDict xs -> FromJust (Get s xs)

instance Accessible s ('(True, t, x) ': xs) where
    get s (TDCons _ _ x _) = x

instance (Accessible s xs) => Accessible s ('(False, t, x) ': xs) where
    get s (TDCons _ _ _ xs) = get s xs

testAccessible0 :: FromJust (Get "B" (TagEq "B" '[ '("A", Char), '("B", Int) ]))
testAccessible0 = get Proxy
                    (TDCons SFalse Proxy 'a'
                        (TDCons STrue Proxy 3 TDNil))

--------------------------------------------------------------------------------
--  Dictionary Declaration
--------------------------------------------------------------------------------

type family Declare (s :: Symbol) (x :: *) (xs :: [ (Symbol, *) ]) :: [ (Symbol, *) ] where
    Declare s x '[]             = '[ '(s, x) ]
    Declare s x ('(t, y) ': xs) = '(t, y) ': (Declare s x xs)

class DeclareClass s x xs where
    declare :: (Member s xs ~ False) => Sym s -> x -> Dict xs -> Dict (Declare s x xs)

instance DeclareClass s x '[] where
    declare s x _ = DCons s x DNil

instance (  Member s xs ~ False
         ,  Declare s x ('(t, y) ': xs) ~ ('(t, y) ': (Declare s x xs))
         ,  DeclareClass s x xs )
         => DeclareClass s x ('(t, y) ': xs) where
    declare s x (DCons t y xs) = DCons t y (declare s x xs)

testDeclare0 :: Dict (Declare "A" Bool '[])
testDeclare0 = declare (Proxy :: Proxy "A") True DNil

testDeclare1 :: Dict (Declare "A" Bool '[ '("B", Int) ])
testDeclare1 = declare (Proxy :: Proxy "A") True (DCons (Proxy :: Proxy "B") 0 DNil)

testDeclare2 :: Dict (Declare "C" Bool '[ '("A", Char), '("B", Int) ])
testDeclare2 = declare (Proxy :: Proxy "C") True dictAB

--------------------------------------------------------------------------------
--  Dictionary Update
--------------------------------------------------------------------------------

type family Update (s :: Symbol) (x :: *) (xs :: [ (Symbol, *) ]) :: [ (Symbol, *) ] where
    Update s x ('(s, y) ': xs) = ('(s, x) ': xs)
    Update s x ('(t, y) ': xs) = '(t, y) ': (Update s x xs)

testUpdateType0 :: (Update "A" Char '[ '("B", Int) ] ~ '[ '("B", Int), '("A", Char) ]) => ()
testUpdateType0 = ()

testUpdateType1 :: (Update "A" Char '[ '("A", Int) ] ~ '[ '("A", Char) ]) => ()
testUpdateType1 = ()

class UpdateClass s x xs where
    update :: (Member s xs ~ True) => Sym s -> x -> Dict xs -> Dict (Update s x xs)

instance UpdateClass s x ('(s, y) ': xs) where
    update s x (DCons _ _ xs) = DCons s x xs

instance (  Member s xs ~ True
         ,  Update s x ('(t, y) ': xs) ~ ('(t, y) ': (Update s x xs))
         ,  UpdateClass s x xs )
         => UpdateClass s x ('(t, y) ': xs) where
    update s x (DCons t y xs) = DCons t y (update s x xs)

testUpdate0 :: Dict (Update "A" Bool '[ '("A", Char), '("B", Int) ])
testUpdate0 = update (Proxy :: Proxy "A") True dictAB

--------------------------------------------------------------------------------
--  Dictionary Deletion
--------------------------------------------------------------------------------

type family Delete (s :: Symbol) (xs :: [ (Symbol, *) ]) :: [ (Symbol, *) ] where
    Delete s ('(s, y) ': xs) = xs
    Delete s ('(t, y) ': xs) = '(t, y) ': (Delete s xs)

testDelType1 :: (Delete "A" '[ '("A", Char) ] ~ '[]) => ()
testDelType1 = ()

testDelType3 :: (Delete "A" '[ '("B", Int), '("A", Char) ] ~ '[ '("B", Int) ]) => ()
testDelType3 = ()

class DeleteClass s xs where
    delete :: (Member s xs ~ True) => Sym s -> Dict xs -> Dict (Delete s xs)

instance DeleteClass s ('(s, x) ': xs) where
    delete s (DCons _ x xs) = xs

instance (   Member s xs ~ True
         ,   Delete s ('(t, x) ': xs) ~ ('(t, x) ': Delete s xs)
         ,   DeleteClass s xs)
         =>  DeleteClass s ('(t, x) ': xs) where
    delete s (DCons t x xs) = DCons t x (delete s xs)

stestDelType0 :: Dict (Delete "A" '[ '("A", Char), '("B", Int) ])
stestDelType0 =  DCons Proxy 3 DNil

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
