{-# LANGUAGE DataKinds, GADTs, TypeOperators, PolyKinds, TypeFamilies, KindSignatures #-}

module Lock where

-- import GHC.TypeLits
-- import Data.Proxy

-- data Locked
-- data Unlocked


data N = Z | S N
data N_ :: N -> * where
    Z_ :: N_ Z
    S_ :: N_ n -> N_ (S n)

toInt :: N_ n -> Int
toInt Z_ = 0
toInt (S_ n) = 1 + toInt n

data Lock = O | X
data Lock_ :: Lock -> * where
    O_ :: Lock_ O
    X_ :: Lock_ X

instance Show (Lock_ a) where
    show O_ = "O"
    show X_ = "X"

data Resources :: [ Lock ] -> * where
    Nil :: Resources '[]
    Cons :: (Lock_ x) -> Resources xs -> Resources (x ': xs)

ex :: Resources [X, O, X]
ex = Cons X_ (Cons O_ (Cons X_ Nil))

type family Get (n :: N) (xs :: [k]) :: k where
    Get Z     (x ': xs) = x
    Get (S n) (x ': xs) = Get n xs

get :: Resources xs -> N_ n -> Lock_ (Get n xs)
get Nil         n = error "empty list"
get (Cons x xs) Z_     = x
get (Cons x xs) (S_ n) = get xs n

type family Set (n :: N) (x :: k) (xs :: [k]) :: [k] where
    Set 'Z     x' (x ': xs) = x' ': xs
    Set ('S n) x' (x ': xs) = Set n x' xs


class PMonad m where
    unit :: a -> m p p a
    bind :: m p q a -> (a -> m q r b) -> m p r b

-- data LockM :: [Lock] -> [Lock] -> * -> * where
--     LockM :: IO a -> LockM p q a

data LockM p q a = LockM { unlockM :: IO a }

instance PMonad LockM where
    unit = LockM . return
    bind m k = LockM (unlockM m >>= unlockM . k)

lput :: String -> LockM p p ()
lput = LockM . putStrLn

acquire :: (Get n xs ~ X) => N_ n -> LockM xs (Set n X xs) ()
acquire n = LockM (putStrLn $ "locking" ++ show (toInt n))

-- release :: N_ n -> LockM xs (Set n X xs) ()
