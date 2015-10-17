{-# LANGUAGE TypeFamilies, DataKinds, GADTs, TypeOperators #-}

-- Fun with type functions
--  Oleg Kiselyov, Simon Peyton Jones, Chung-chieh Shan
--  May 3, 2010
-- http://research.microsoft.com/en-us/um/people/simonpj/papers/assoc-types/fun-with-type-funs/typefun.pdf


module TypeFamilies where

import Data.IORef
import Data.STRef
import Control.Monad.ST

class Mutation m where
    type Ref m :: * -> *
    newRef :: a -> m (Ref m a)
    readRef :: Ref m a -> m a
    writeRef :: Ref m a -> a -> m ()

instance Mutation IO where
    type Ref IO = IORef
    newRef = newIORef
    readRef = readIORef
    writeRef = writeIORef

instance Mutation (ST s) where
    type Ref (ST s) = STRef s
    newRef = newSTRef
    readRef = readSTRef
    writeRef = writeSTRef


--------------------------------------------------------------------------------
--  Nat (type level)
--------------------------------------------------------------------------------
--  type term   term
data Nat = Zero | Succ Nat

-- example
n3 :: Nat
n3 = Succ (Succ (Succ Zero))



--------------------------------------------------------------------------------
--  Nat (term level), indexed by type level Nat
--------------------------------------------------------------------------------
--   type   kind -> kind
data SNat :: Nat -> * where
    SZero :: SNat 'Zero
    SSucc :: SNat n -> SNat ('Succ n)

-- example
sn3 :: SNat (Succ (Succ (Succ Zero)))
sn3 = SSucc (SSucc (SSucc SZero))

--------------------------------------------------------------------------------
--  Parameterized Monad
--------------------------------------------------------------------------------

class PMonad m where
    unit :: a -> m p p a
    bind :: m p q a -> (a -> m q r b) -> m p r b

--------------------------------------------------------------------------------
--  Lock
--------------------------------------------------------------------------------

data Locked
data Unlocked

data LockM p q a = LockM { unLockM :: IO a }

instance PMonad LockM where
    unit = LockM . return
    bind m f = LockM (unLockM m >>= unLockM . f)


lput :: String -> LockM p p ()
lput = LockM . putStrLn


-- predS :: Succ n -> n
-- predS = undefined

-- class Nat n where
--     toInt :: n -> Int
--
-- instance Nat Zero where
--     toInt _ = 0
--
-- instance (Nat n) => Nat (Succ n) where
--     toInt m = 1 + toInt (predS m)
--

-- type family Get :: Nat -> [ * ] -> *
-- type instance Get Zero (x ': xs) = x
-- type instance Get (Succ n) (x ': xs) = Get n xs
--
-- type family Set n x p
-- type instance Set Zero x' (x ':  xs) = x' ':  xs
-- type instance Set (Succ n) x' (y ':  xs) = Set n x' xs
--
-- data Lock n = Lock Int deriving Show
--
-- mkLock :: Nat n =>  -> Lock n
-- mkLock = Lock (toInt (undefined :: n))
