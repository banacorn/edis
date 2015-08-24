{-# LANGUAGE DataKinds, KindSignatures, GADTs, TypeOperators,
     PolyKinds, TypeFamilies #-}

module TypeSafe4 where

data HList :: [ * ] -> * where
  HNil :: HList '[]
  HCons :: a -> HList ts -> HList (a ': ts)

data Nat = Zero | Suc Nat

type family   Nth (n :: Nat) (xs :: [k]) :: k
type instance Nth 'Zero    (x ': xs) = x
type instance Nth ('Suc n) (x ': xs) = Nth n xs

{-

In a mixed-phase language...``

(!!) :: Nat -> [a] -> a

nth :: (n :: Nat) -> HList ts -> ts !! n
nth Zero (HCons x xs) = x
nth (Suc n) (HNil x xs) = nth n xs
-}


data SNat :: Nat -> * where
  SZero :: SNat 'Zero
  SSuc  :: SNat n -> SNat ('Suc n)

nth :: SNat n -> HList ts -> Nth n ts
nth SZero (HCons x _) = x
nth (SSuc n) (HCons _ xs) = nth n xs

type family   Repl (n :: Nat) (y :: k) (xs :: [k]) :: [k]
type instance Repl 'Zero    y (x ': xs) = (y ': xs)
type instance Repl ('Suc n) y (x ': xs) = x ':  Repl n y xs

repl :: SNat n -> b -> HList ts -> HList (Repl n b ts)
repl SZero    y (HCons _ xs) = HCons y xs
repl (SSuc n) y (HCons x xs) = HCons x (repl n y xs)

class Monadish m where
  gret  :: a -> m p p a
  gbind :: m p q a -> (a -> m q r b) -> m p r b

newtype VST m p q a = VST { unVST :: p -> m (a, q) }

instance Monad m => Monadish (VST m) where
  gret x = VST (\s -> return (x, s))
  (VST m) `gbind` f =
    VST (\s0 -> do (a, s1) <- m s0
                   unVST (f a) s1)

vget :: Monad m => SNat n -> VST m (HList ts) (HList ts) (Nth n ts)
vget i = VST (\ss -> return (nth i ss, ss))

vput :: Monad m => SNat n -> b -> VST m (HList ts) (HList (Repl n b ts)) ()
vput i y = VST (\ss -> return ((), repl i y ss))

prog :: Monad m => VST m (HList '[Char, Bool, Int]) (HList '[Char, Int, Int]) Bool
prog = vget foo      `gbind` \b ->
       vput foo True `gbind` \() ->
       vput foo 3    `gbind` \() ->
       vget foo      `gbind` \n ->
       gret (even n && b)
  where foo = SSuc SZero
