
 On Aug 13, 2015, at 10:56 AM, Tyng-Ruey Chuang <trc@iis.sinica.edu.tw> wrote:
 Shall we meet again next TUESDAY morning at 10 am?

Yes, let's meet. :)

 I think I am confused by the term shallow embedding and deep embedding
 of DSL data types into the Haskell type system.

 I believe what 廷彥 is doing shall allow for more flexible representations
 of DSL data types in Haskell. Also, what is a Redis key is mapped to
 a string value (which is used to maintain an internal key-type associative
 list) in a Haskell program. Yours seem to map Redis keys directly to Haskell
 value identifiers.



Yes, it has to be in the system to be statically checked.
And, very often, one ends up using one form of de-Bruin index or
another. It happens with monad transformers too.. There might be
a better way to deal with naming, but I have to check.

I did some quick survey and it turns out that Oleg Kiselyov did
something similar years ago. Here is another literate Haskell
transcript...

> module TypeSafe2 where
> import Control.Monad.State

A "monadish" class, from Oleg Kiselyov. It captures the general
notion of Hoare-triples: in m p q a, p is a precondition before
execution, and q is a post-condition.

> class Monadish m where
>    gret :: a -> m p p a
>    gbind :: m p q a -> (a -> m q r b) -> m p r b

The following type, VS, is a state monad, where the type of the
state can be altered. In (VS si so a), si is the type of state before
execution, while so is the type after execution.

This is slightly different from our application -- our state
are stored in the database rather than in the code. But it serves
as an example:

> newtype VS si so a = VS { runVS :: si -> (a , so) }

The gret and gbind methods are as one would expect:

> instance Monadish VS where
>  gret v = VS (\s -> (v, s))
>  (VS m) `gbind` f =
>    VS (\si -> let (a, s) = m si
>               in runVS (f a) s    )

while vsget and vsput are lifted get and put. Notice the
change of type of the state in vsput:

> vsget :: VS s s s
> vsget = VS (\s -> (s, s))

> vsput :: so -> VS si so ()
> vsput so = VS (\_ -> ((), so))

One can also wrap an existing monad into a VS (see attached).

To have more than one variables, we might try to turn VST
into a Monadish-transformer. One of the usual ways to handle
multiple states is to stack the states, and use lifting when
you access the stack... which is like de-Bruin index. People
hate it, but still end up using it in production code. I'll
do some experiments and see whether it works.

sincerely,
Shin
scm@iis.sinica.edu.tw


A variable-typed-state monad transformer
(well, not quite, since VST m is not a monad, but more general.
We will need a Monadish-to-Monadish transformer.)

> newtype VST m si so a = VST { runVST :: si -> m (a , so) }

> instance Monad m => Monadish (VST m) where
>  gret v = VST (\s -> return (v, s))
>  (VST m) `gbind` f =
>    VST (\si -> do (a, s) <- m si
>                   runVST (f a) s    )

> vstget :: Monad m => VST m s s s
> vstget = VST (\s -> return (s, s))

> vstput :: Monad m => so -> VST m si so ()
> vstput so = VST (\_ -> return ((), so))
