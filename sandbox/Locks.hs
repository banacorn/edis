{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

-- Using parameterized (generalized) `monad' to statically enforce
-- lock safety in advanced cases.
--
-- Lock safety means: no held lock may be acquired, no free
-- lock may be freed, all locks must be freed by the end
-- of the computation.
--
-- The simple case of the problem, for one lock, is implemented in
-- http://okmij.org/ftp/Computation/monads.html#param-monad
-- http://okmij.org/ftp/Haskell/DoParamM.hs
--
-- This file deals with the case of multiple locks. For simplicity and
-- clarity, we consider only two locks. Generalization to more than
-- two is straightforward, but requires the type-level programming
-- similar to that of the HList library.
--
-- The first example in this file illustrates acquiring several locks
-- at once. The locks can be released by one unlock command, or
-- one at a time. Any order of lock acquisition and release is supported
-- as long as lock safety holds.
--
-- The second example illustrates conditional acquisition and release
-- of locks, based on a run-time condition. Although we cannot know
-- at compile time the value of the condition, we statically ensure
-- lock safety nevertheless.
--
-- The code was written in April 2008. It is ported to GHC 7.0,
-- taking advantage of the rebindable syntax and type families.

module Locks where

import qualified Prelude
import Prelude hiding (return, (>>=), (>>), fail)

-- A parameterized `monad'
class Monadish m where
    return :: a -> m p p a
    fail   :: String -> m p p a
    (>>=)  :: m p q a -> (a -> m q r b) -> m p r b

m1 >> m2 = m1 >>= (const m2)

-- All regular monads are parameterized `monads'
newtype RegularM m p q a = RegularM{unRM :: m a}

instance Prelude.Monad m => Monadish (RegularM m) where
    return = RegularM . Prelude.return
    fail   = RegularM . Prelude.fail
    m >>= f = RegularM ((Prelude.>>=) (unRM m) (unRM . f))

-- Needed if the rebindable syntax is used
ifThenElse e1 e2 e3 = case e1 of True -> e2; _ -> e3


type family Apply f x :: *

-- ------------------------------------------------------------------------
-- First example: multiple locks and locking several locks at once.
-- For clarity, we limit ourselves here to two locks. The technique
-- is general however, and can support an arbitrary number of locks:
-- the state of the generalized monad becomes then an HList (nested
-- tuple, essentially) of the types of the held locks. We would use
-- the HList library for operations such as membership testing, deletion,
-- insertion, etc. on these HLists.

-- Begin the library code. The type functions Insert and
-- Delete are used only internally

data N1 = N1 deriving Show		-- Avoid Peano numerals for simplicity
data N2 = N2 deriving Show

-- A trivial type equality predicate, most naive but simple.
-- HList has a predicate that takes only a few lines and works for all
-- types, present and declared in the future. That predicate takes
-- pages to explain. Hopefully GHC might provide a similar predicate
-- internally

type family TEQ a b :: *		-- actually, either HTrue or HFalse

data HTrue
data HFalse

-- GHC definitely will provide the equality comparison on
-- type-level numerals, soon. The following lines will become
-- obsolete then.

type instance TEQ N1 N1 = HTrue
type instance TEQ N2 N2 = HTrue
type instance TEQ N1 N2 = HFalse
type instance TEQ N2 N1 = HFalse


-- Locks, indexed by some type (usually, a numeral)
-- We lift the equality of the index types to locks
newtype Lock a = Lock a deriving Show
type instance TEQ (Lock a) (Lock b) = TEQ a b


-- Here we define a tuple HL2 to emulate an HList of at most two elements
-- and the basic operations on that list. The implementation is the most naive.
-- But it also the most straightforward and so abstracts from the
-- complexity of the HList library

data HL0     = HL0      deriving Show
data HL1 a   = HL1 a    deriving Show
data HL2 a b = HL2 a b  deriving Show

-- insert an element a into l1 yielding l2. The list should not already contain
-- the element a
type family Insert a l :: *
data InsertF

type instance Insert (Lock a) HL0     = HL1 (Lock a)
type instance Insert (Lock a) (HL1 y) =
    Apply InsertF (TEQ (Lock a) y, (Lock a), HL1 y)
type instance Apply InsertF (HFalse, x, HL1 y) = HL2 x y

-- delete an existing element a from l1 yielding l2.

type family Delete a l :: *
data DeleteF

type instance Delete (Lock a) (HL1 (Lock a)) = HL0
type instance Delete (Lock a) (HL2 b c)      =
    Apply DeleteF (TEQ (Lock a) b, Lock a, HL2 b c)
type instance Apply DeleteF (HTrue, Lock a, HL2 (Lock a) c) = HL1 c
type instance Apply DeleteF (HFalse, Lock a, HL2 b c) =
    Apply DeleteF (TEQ (Lock a) c, HL2 b c)
type instance Apply DeleteF (HTrue, HL2 b c) = HL1 b


newtype LIO p q a = LIO{unLIO::IO a}

-- The parameterized `monad' that carries the locking state in its _phantom_
-- parameters i (the state before the action) and o (the state after the
-- action)
instance Monadish LIO where
  return  = LIO . Prelude.return
  m >>= f = LIO ((Prelude.>>=) (unLIO m) (unLIO . f))

-- Lift a few i/o operations to LIO
lput :: String -> LIO p p ()
lput = LIO . putStrLn
lget :: LIO p p String
lget = LIO getLine

-- In the real program, the following will execute actions to acquire
-- or release a lock. Here, we just print out our intentions.
lock :: Show l => l -> LIO i (Insert l i) ()
lock l = LIO (putStrLn $ "Lock " ++ show l)

unlock :: Show l => l -> LIO i (Delete l i) ()
unlock l = LIO (putStrLn $ "UnLock " ++ show l)

-- We should start in unlocked state, and finish in the same state
runLIO :: LIO HL0 HL0 a -> IO a
runLIO = unLIO

nolocks :: LIO HL0 HL0 ()
nolocks = LIO (Prelude.return ())

-- The user code

lock1 = Lock N1				-- useful abbreviations
lock2 = Lock N2

tlock1 = do
 l <- lget
 x <- return (read l) -- emulates `processing' of the read line l
 lput (show (x+1))

tlock1r = runLIO tlock1
-- If we enter 1, we get the printed result 2

{-
  *Locks> :t tlock1
  tlock1 :: LIO r r ()
 Inferred type has the same input and output states and is polymorphic:
 tlock1 does not affect the state of the lock.
-}


tlock2 = do
  l <- lget
  lock lock1
  x <- return (read l)
  lput (show (x+1))

{-
  *Locks> :t tlock2
  tlock2 :: LIO q (Insert (Lock N1) q) ()

 The inferred type says that the tlock2 holds the lock1 when terminates.
-}


-- tlock2r = runLIO tlock2
{- If uncommented, it gives the type error
    Couldn't match type `HL1 (Lock N1)' with `HL0'
    Expected type: LIO HL0 HL0 ()
      Actual type: LIO HL0 (Insert (Lock N1) HL0) ()
    In the first argument of `runLIO', namely `tlock2'

 That is, impossible to run code that has not freed Lock1
-}


tlock22 = lock lock2 >> tlock2
tlock23r = runLIO (tlock22  >> unlock lock1 >> unlock lock2)
{-
*Locks> tlock23r
Lock Lock N2
<enter 1>
Lock Lock N1
2
UnLock Lock N1
UnLock Lock N2
-}

tlock3 = tlock2 >> unlock lock2
-- tlock3 :: LIO p (Delete (Lock N2) (Insert (Lock N1) p)) ()

tlock3' = tlock2 >> unlock lock1
-- tlock3' :: LIO p (Delete (Lock N1) (Insert (Lock N1) p)) ()

tlock3r' = runLIO tlock3'

{-
*Locks> tlock3r'
-- user input: 123
Lock Lock N1
124
UnLock Lock N1
-}

-- An attempt to execute the following
tlock4 = tlock2 >> tlock2
-- tlock4 :: LIO p (Insert (Lock N1) (Insert (Lock N1) p)) ()

-- We can't run that code however:
{-
tlock4r = runLIO tlock4

    Expected type: LIO HL0 HL0 ()
      Actual type: LIO HL0 (Insert (Lock N1) (Insert (Lock N1) HL0)) ()

I admit the error message could be clearer about the problem of
trying to acquire the already held lock. There is something we can do
to make the error message clearer, but it complicates the code.
-}

-- The following is OK
tlock4' = tlock2 >> lock lock2
-- tlock4' :: LIO p (Insert (Lock N2) (Insert (Lock N1) p)) ()

-- and so is the following
tlock4'' = tlock2 >> unlock lock1 >> tlock2
-- The inferred type clearly shows the sequence of locking actions
{-
tlock4''
  :: LIO p (Insert (Lock N1) (Delete (Lock N1) (Insert (Lock N1) p))) ()
-}
tlock4''r = runLIO (tlock4'' >> unlock lock1)


-- Similarly, the following gives a type error because of an attempt
-- to release a lock twice
tlock4''' = tlock2 >> unlock lock1 >> unlock lock1
{-
tlock4'''
  :: LIO
       p (Delete (Lock N1) (Delete (Lock N1) (Insert (Lock N1) p))) ()

-}
-- But we can't run the code
-- One may wish for a bit eager constraint resolution
{-
tlock4'''r = runLIO tlock4'''
    Couldn't match type `Delete (Lock N1) HL0' with `HL0'
-}


-- We can add two locks at once
-- We extend our implementation as follows, to permit insertion and
-- deletion of a list of locks

type instance Insert HL0 l       = l
type instance Insert (HL1 a) l   = Insert a l
type instance Insert (HL2 a b) l = Insert b (Insert a l)

type instance Delete HL0 l       = l
type instance Delete (HL1 a) l   = Delete a l
type instance Delete (HL2 a b) l = Delete b (Delete a l)

-- That was it. End of the changes to the implementation

-- User code: tests

tlock12 = do
  l <- lget
  lock (HL2 lock1 lock2)
  x <- return (read l)
  lput (show (x+1))

{-
 *Locks> :t tlock12
 tlock12 :: LIO q (Insert (Lock N2) (Insert (Lock N1) q)) ()

The inferred type says two locks are held at the end of tlock12
-}


-- tlock12r = runLIO tlock12
{- The above, if uncommented, gives a type error:

    Couldn't match type `HL2 (Lock N2) (Lock N1)' with `HL0'
    Expected type: LIO HL0 HL0 ()
      Actual type: LIO HL0 (Insert (Lock N2) (Insert (Lock N1) HL0)) ()

 Two locks are not released.
-}

-- tlock12r1 = runLIO (tlock12 >> unlock lock1)
{-
 Still a type error
    Couldn't match type `HL1 (Lock N2)' with `HL0'

lock2 is still being held
-}

-- This is OK: we acquire two locks at once but release them separately
tlock12r2 = runLIO (tlock12 >> unlock lock2 >> unlock lock1)

{-
*Locks> tlock12r2
-- user input: 123
Lock HL2 (Lock N1) (Lock N2)
124
UnLock Lock N2
UnLock Lock N1
-}


-- These are OK too: the order of release does not matter
tlock12r3 = runLIO (tlock12 >> unlock (HL2 lock2 lock1))
tlock12r4 = runLIO (tlock12 >> unlock (HL2 lock1 lock2))

-- ------------------------------------------------------------------------
-- Second example: Locking either lock1 or lock2 based on a run-time value
-- We still _statically_ assure lock safety.
-- See the `user code' below, in particular, rtcl3.

-- We extend our implementation again. We use fresh types (eigenvariables)
-- to represent unknown dynamic values.

-- A `typed boolean'
-- s is the type (eignevariable) that represents the dynamic boolean
newtype TBool s    = TBool Bool
newtype TBoolNeg s = TBoolNeg Bool	-- represents the opposite boolean

-- generating a unique s to correspond to the boolean value
brand_bool :: Bool -> (forall s. TBool s -> TBoolNeg s -> w) -> w
brand_bool b f = f (TBool b) (TBoolNeg b)

-- Negating the type boolean, preserving the type s
class Neg a where
    type Negated a :: * -> *
    neg :: a s -> Negated a s

instance Neg TBool where
    type Negated TBool = TBoolNeg
    neg (TBool b) = TBoolNeg b

instance Neg TBoolNeg where
    type Negated TBoolNeg = TBool
    neg (TBoolNeg b) = TBool b


-- We use a special data type: a conditional lock. For simplicity,
-- we use only one conditional lock. Multiple conditional locks can
-- be handled using the techniques of the Lightweight Monadic Regions paper

-- s is the typed boolean that selects either the lock a or the lock b
-- We can't say which lock is selected as the value of the typed boolean
-- is not statically known
data CL s a b = CL s a b

-- Operations to acquire and release a lock conditionally.
-- Our type state contains not only the name of the lock but also
-- the `name' of the condition. The lock acquired under a condition
-- must be released under the same condition. That statically assures
-- the lock safety.

class CLock s l i where
    type ClockT s l i :: *
    clock   :: s -> l -> LIO i (ClockT s l i) ()
    cunlock :: s -> l -> LIO (ClockT s l i) i ()

instance Show a => CLock (TBool s) (Lock a) HL0 where
  type ClockT (TBool s) (Lock a) HL0 = CL (TBool s) (Lock a) ()
  clock (TBool b) l =
	      if b then LIO (putStrLn $ "Lock " ++ show l)
		   else LIO (Prelude.return ())
  cunlock (TBool b) l =
	      if b then LIO (putStrLn $ "Unlock " ++ show l)
		   else LIO (Prelude.return ())

instance Show a => CLock (TBoolNeg s) (Lock a) HL0 where
  type ClockT (TBoolNeg s) (Lock a) HL0 = CL (TBool s) () (Lock a)
  clock (TBoolNeg b) l =
	      if b then LIO (Prelude.return ())
		   else LIO (putStrLn $ "Lock " ++ show l)
  cunlock (TBoolNeg b) l =
	      if b then LIO (Prelude.return ())
		   else LIO (putStrLn $ "Unlock " ++ show l)

instance Show a => CLock (TBool s) (Lock a) (CL (TBool s) () b) where
  type ClockT (TBool s) (Lock a) (CL (TBool s) () b) =
      NormCL (CL (TBool s) (Lock a) b)
  clock (TBool b) l =
	      if b then LIO (putStrLn $ "Lock " ++ show l)
		   else LIO (Prelude.return ())
  cunlock (TBool b) l =
	      if b then LIO (putStrLn $ "Unlock " ++ show l)
		   else LIO (Prelude.return ())

instance Show a => CLock (TBoolNeg s) (Lock a) (CL (TBool s) b ()) where
  type ClockT (TBoolNeg s) (Lock a) (CL (TBool s) b ()) =
      NormCL (CL (TBool s) b (Lock a))
  clock (TBoolNeg b) l =
	      if b then LIO (Prelude.return ())
		   else LIO (putStrLn $ "Lock " ++ show l)
  cunlock (TBoolNeg b) l =
	      if b then LIO (Prelude.return ())
		   else LIO (putStrLn $ "Unlock " ++ show l)

-- We do our own constraint simplification
-- The same lock acquired under a condition and its negation does
-- not actually depend on the condition: P v ~P == T
type family NormCL a :: *
data NormCLF
type instance NormCL (CL s a b) = Apply NormCLF (TEQ a b, CL s a b)
type instance Apply NormCLF (HTrue, CL s a a) = HL1 a
type instance Apply NormCLF (HFalse,CL s a b) = CL s a b

type instance TEQ (Lock a) () = HFalse
type instance TEQ () (Lock a) = HFalse

-- User code: tests

-- Here we see the drawback of the quantified variable s:
-- We have to specify what the input type state is (using nolocks, for example)
-- Instead of quantification, we could have added a new component to the
-- typestate, a type counter to generate fresh types to name the conditional
-- That makes the code more complex however.
tcl1 = do
  nolocks
  l <- lget
  brand_bool ((read l) > (0::Int)) (\b bnot -> do
    clock b    lock1
    clock bnot lock1
    lput l)

{-
 *Locks> :t tcl1
 tcl1 :: LIO HL0 (HL1 (Lock N1)) ()

The inferred type says that Lock1 is being held unconditionally: indeed,
the same lock is being acquired with under the condition and its negation
-}

-- The following is OK: we can use an unconditional operation to release
-- the lock
tcl1r = runLIO (tcl1 >> unlock lock1)


{-
tcl2 = do
  l <- lget
  brand_bool ((read l) > (0::Int)) (\b bnot -> do
    clock b    lock1
    clock bnot lock2
    lput l)
-}

{-
The above code, if uncommented, fails to type check. If we attempt
to give a signature, we see the problem, which is quite subtle:

  tcl2 :: LIO HL0 (CL (TBool s) (Lock N1) (Lock N2)) ()

The signature says that at the end of tcl2, either lock 1 is held
or lock 2, depending on the dynamic input. The problem is in the escaping
quantified variable s. The conditions b and bnot are not in scope any more
at the end of tcl2. Therefore, we could never release the locks.

The type-checker rejects the code. Very clever!
-}


{-
rtcl2 = runLIO $ do
  l <- lget
  brand_bool ((read l) > (0::Int)) (\b bnot -> do
    clock b lock1
    clock bnot lock2
    lput l
    cunlock b lock1)
-}

{- The above, if uncommented, gives a type error:
    Couldn't match type `Lock N2' with `()'
    Expected type: LIO
                     (CL (TBool s) (Lock N1) ()) (CL (TBool s) (Lock N1) ()) ()
      Actual type: LIO
                     (CL (TBool s) (Lock N1) ())
                     (ClockT (TBoolNeg s) (Lock N2) (CL (TBool s) (Lock N1) ()))
                     ()

The error message essentially says that Lock2 is still being held,
conditionally. One cannot statically prove lock safety. Indeed, there
exists a program run that violates the safety.
-}


{-
rtcl2 = runLIO $ do
  l <- lget
  brand_bool ((read l) > (0::Int)) (\b bnot -> do
    clock b lock1
    clock bnot lock2
    lput l
    cunlock b lock1
    cunlock b lock2)

-}

{- The above, if uncommented, gives a type error:
    No instance for (CLock
                       (TBool s) (Lock N1) (CL (TBool s) (Lock N2) ()))
      arising from a use of `cunlock'

We try to remove both locks. However, lock2 was acquired under the negation
of the condition and is being released under the original condition.
-}

-- But the following is OK
rtcl3 = runLIO $ do
  l <- lget
  brand_bool ((read l) > (0::Int)) (\b bnot -> do
    clock b lock1
    clock bnot lock2
    lput l
    cunlock b lock1
    cunlock bnot lock2)

-- The following shows that depending on the input value, either lock1
-- is acquired and released, or lock2. The lock safety holds in either
-- case.
{-
*Locks> rtcl3
-- user input: 1
Lock Lock N1
1
Unlock Lock N1
*Locks> rtcl3
-- user input: 0
Lock Lock N2
0
Unlock Lock N2
-}
