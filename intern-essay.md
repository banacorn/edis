# Type-safe Redis

Redis is an open source key-value store. It supports various of data structure such as strings, hashes, lists, sets, etc. Redis is itself a standalone server (`redis-server`), to manipulate data stored in Redis, we can send commands from clients (`redis-cli`).

```
REDIS> SET key value
OK
REDIS> GET key
"value"
REDIS> SET key foo
OK
REDIS> GET key
"foo"
```

We can also write programs to manipulate Redis. Most programming languages out there have bindings available, and the one we are tackling on is Haskell's `Redis`

```haskell
runRedis conn $ do
     set "hello" "hello"
     set "world" "world"
     hello <- get "hello"
     world <- get "world"
     liftIO $ print (hello,world)
```

It's pretty much the same as in `redis-cli`.

## Everything is a string

Keys in Redis are just plain strings of arbitrary length, and they are binary-safe, that means you can even use a JPG image as a key. Although Redis supports many different kinds of data structures, they are essentially all just strings. For example, a key can be paired with a string, a list of strings, or a set of strings, and so on.

## Not all strings are the same

Some commands modifies the content of a value. For example, the command `INCR`, as its name suggests, increments a value by 1.

```
REDIS> SET n 42
REDIS> INCR n
REDIS> GET n
"43"
```

Notice those values just happened to look like numbers, these "numbers" are still strings. And if we apply `INCR` to some value that doesn't look like a number, it fails.

```
REDIS> SET n banana
OK
REDIS> INCR n
(error) ERR value is not an integer or out of range
```

All strings are equal, but some strings are more equal than others.

If we run the previous example with `hedis` in Haskell

```haskell
runRedis conn $ do
     set "n" "banana"
     incr "n"
     n <- get "n"
     liftIO $ print n
```

```
Right (Just "banana")
```

It fails silently, like it never happened. It will be better if we can do some semantics checking, and prevents ill-formed commands from being executed.

## Give Redis some types

Some claim that dynamically typed languages are in reality statically typed, uni-typed languages. In the Redis world, there's only one type, namely *String*.

Since the command `INCR` only works on certain subset of *String* (base-10 64 bit signed integer), if we can give these strings a new type, say *Integer*, then we can associate those value-paired keys with types. And restrict the domain of some commands, to only certain types, for example, `INCR` now only applies to keys with value of *Integer* type.

## Give Redis some more types

It wouldn't be darn interesting, if we are type checking a language with only 2 types.

In practice, when working with Redis in Haskell, people often serialize their data of some type into raw bytestrings, store them in Redis, and deserialize them after retrieval.


If we can know the type of data before serialization, then we can associate the type with its key!

In Haskell, we can obtain some type information of a value in runtime, if the value is *Typeable*. The Typeable class, from the module `Data.Typeable`, reifies types to some extent by associating type representations to types.

The module comes with some handy functions:

```haskell
typeRep :: forall proxy a. Typeable a => proxy a -> TypeRep
typeOf :: forall a. Typeable a => a -> TypeRep
```

And we can store these `Key` - `TypeRep` relations somewhere in a state monad

```haskell
type Key = ByteString
type TypeTable = Map Key TypeRep

data TxState = TxState { typeTable :: TypeTable}
type Tx = State TxState
```

## Dynamic type checking

With key-value pairs typed, we can now check if our commands are well-formed.

These are some helper functions for key-type relation maintenance.
```haskell
insertType :: Key -> TypeRep -> Tx ()
lookupType :: Key -> Tx (Maybe TypeRep)
removeType :: Key -> Tx ()
```

Type error:
* `Undeclared`: if there's no such key-type relation stored in our table.
* `TypeMismatch`: when its original key-type relation is violated.

```haskell
data TypeError = Undeclared Key | TypeMismatch Key TypeRep TypeRep

checkType :: Key -> TypeRep -> Tx (Maybe TypeError)
checkType key got = do
    result <- lookupType key
    case result of
        Nothing -> do
            return $ Just (Undeclared key)
        Just expected -> if expected == got
            then return $ Nothing
            else return $ Just (TypeMismatch key expected got)
```

Now the command `INCR` look like these:
```haskell
incr :: Key -> Tx ()
incr key = do
    typeError <- checkType key (typeRep (Proxy :: Proxy Int))
    case typeError of
        Just err -> assertError err
        Nothing -> sendCommand ["INCR", key]
```

The program below will not be issued to the Redis server, since there is a type error.

```haskell
runTx conn $ do
     set "n" ("banana" :: String)
     incr "n"
```

## Static type checking

Dynamic type checking prevents programmer from issueing ill-formed Redis commands, but it has some downsides, for example the overhead of type checking at runtime.

The ultimate goal is to have our Redis progamm statically type checked. To do so we'll need to "promote" the key-type relations to the type-level, albeit various dependent type related techniques and language extensions introduced in recent years.

It's still unknown if static type checking is feasible and practical. But we will keep working on it, even after the internship.
