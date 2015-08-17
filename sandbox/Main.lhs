Dear friends,

Something I quickly composed this afternoon, regarding statically
type-checked embedded languages. For now we consider deeply embedded
languages (I presume that's also our main concern how, is it?)

This is a literate Haskell script. The followings are the Haskell compiler
flags I needed:

> {-# LANGUAGE GADTs, ScopedTypeVariables, DataKinds,
>              TypeOperators, KindSignatures #-}

Consider first the simple case where there is only one variable,
with two operations. "Get" retrieves the value of that variable,
while "Coerce" converts the type of that variable to something else.
They form a monadic-ish structure like this:

> data M (s :: *) (t :: *) (a :: *) where
>   Return :: a -> M s t a
>   Bind   :: M s t a -> (a -> M t u b) -> M s u b
>    --
>   Coerce :: M s t ()
>   Get    :: M t t t

In M s t a, s is the type of the variable before execution,
t is the type after execution, and a is the type of the result.

We may compose programs like this:

> p :: M Int Char ()
> p = Get                          `Bind` \ (n :: Int) ->
>     coerceTo (undefined :: Bool) `Bind` \ () ->
>     Get                          `Bind` \ (b :: Bool) ->
>     Return ()

> coerceTo :: a -> M s a ()
> coerceTo _ = Coerce

The program would not type check if you, for example, change
 (b :: Bool) to  (b :: Char).

Consider the case when there are more than one variables.
We keep the types of variables in a list. The following type
(Mem a ts) is a witness that a is a member of the list ts:

> data Mem (a :: *) (ts :: [*]) where
>  Zero :: Mem a (a ': ts)
>  Suc :: Mem a ts -> Mem a (b ': ts)

while (Repl a ts b us) is a witness that "if you replace the
type 'a' in ts to 'b', you get 'us'.":

> data Repl (a :: *) (ts :: [*]) (b :: *) (us :: [*]) where
>  Zero' :: Repl a (a ': ts) b (b ': ts)
>  Suc'  :: Repl a ts b us -> Repl a (c ': ts) b (c ': us)

The abstract syntax is now like:

> data Ms (s :: [*]) (t :: [*]) (a :: *) where
>   ReturnM :: a -> Ms s t a
>   BindM   :: Ms s t a -> (a -> Ms t u b) -> Ms s u b
>   Gets    :: Mem a s -> Ms s s a
>   Coerces :: Repl a ts b us -> Ms ts us ()

And the following program assumes an environment with three
variables, of type [Int, Bool, Char]. It reads the second
variable, change its type to Char, and read it again:

> p2 :: Ms '[Int, Bool, Char] '[Int, Char, Char] ()
> p2 = Gets (Suc Zero)      `BindM`  \ (b :: Bool) ->
>      Coerces (Suc' Zero') `BindM` \ () ->
>      Gets (Suc Zero)      `BindM` \ (_ :: Char) ->
>      ReturnM ()

There should be better ways to do this, and believe it
has been done before. Gotta check them out.

sincerely,
Shin
scm@iis.sinica.edu.tw
