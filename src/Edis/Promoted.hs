{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, OverloadedStrings #-}

module Edis.Promoted where

import Edis.Type

import Data.Bifunctor           (first)
import Data.ByteString          (ByteString)
import Data.Maybe               (fromJust)
import Data.Proxy               (Proxy)
import Database.Redis as Redis hiding (decode)
import Data.Serialize           (Serialize, encode, decode)
import Data.Type.Bool
import Data.Type.Equality
import GHC.TypeLits

--------------------------------------------------------------------------------
--  Helper functions
--------------------------------------------------------------------------------

encodeKey :: KnownSymbol s => Proxy s -> ByteString
encodeKey = encode . symbolVal

start :: Edis '[] '[] ()
start = Edis (return ())

--------------------------------------------------------------------------------
--  Declaration
--------------------------------------------------------------------------------

declare :: (KnownSymbol s, Member xs s ~ False)
        => Proxy s -> Proxy x -> Edis xs (Set xs s x) ()
declare s x = Edis $ return ()

renounce :: (KnownSymbol s, Member xs s ~ True)
        => Proxy s -> Edis xs (Del xs s) ()
renounce s = Edis $ return ()

--------------------------------------------------------------------------------
--  Hashes
--------------------------------------------------------------------------------

hdel :: (KnownSymbol k, KnownSymbol f, HashOrNX xs k)
        => Proxy k -> Proxy f
        -> Edis xs (DelHash xs k f) (Either Reply Integer)
hdel key field = Edis $ Redis.hdel (encodeKey key) [encodeKey field]

hexists :: (KnownSymbol k, KnownSymbol f, HashOrNX xs k)
        => Proxy k -> Proxy f
        -> Edis xs xs (Either Reply Bool)
hexists key field = Edis $ Redis.hexists (encodeKey key) (encodeKey field)

hget :: (KnownSymbol k, KnownSymbol f, Serialize x
        , Just (StringOf x) ~ GetHash xs k f)
        => Proxy k -> Proxy f
        -> Edis xs xs (Either Reply (Maybe x))
hget key field = Edis $ Redis.hget (encodeKey key) (encodeKey field) >>= decodeAsMaybe

hincrby :: (KnownSymbol k, KnownSymbol f, HashOrNX xs k)
        => Proxy k -> Proxy f -> Integer
        -> Edis xs (SetHash xs k f Integer) (Either Reply Integer)
hincrby key field n = Edis $ Redis.hincrby (encodeKey key) (encodeKey field) n

hincrbyfloat :: (KnownSymbol k, KnownSymbol f, HashOrNX xs k)
        => Proxy k -> Proxy f -> Double
        -> Edis xs (SetHash xs k f Double) (Either Reply Double)
hincrbyfloat key field n = Edis $ Redis.hincrbyfloat (encodeKey key) (encodeKey field) n

hkeys :: (KnownSymbol k, HashOrNX xs k)
        => Proxy k
        -> Edis xs xs (Either Reply [ByteString])
hkeys key = Edis $ Redis.hkeys (encodeKey key)

hlen :: (KnownSymbol k, HashOrNX xs k)
        => Proxy k
        -> Edis xs xs (Either Reply Integer)
hlen key = Edis $ Redis.hlen (encodeKey key)

hset :: (KnownSymbol k, KnownSymbol f, Serialize x, HashOrNX xs k)
        => Proxy k -> Proxy f -> x
        -> Edis xs (SetHash xs k f (StringOf x)) (Either Reply Bool)
hset key field val = Edis $ Redis.hset (encodeKey key) (encodeKey field) (encode val)

hsetnx :: (KnownSymbol k, KnownSymbol f, Serialize x, HashOrNX xs k)
        => Proxy k -> Proxy f -> x
        -> Edis xs (If (MemHash xs k f) xs (SetHash xs k f (StringOf x))) (Either Reply Bool)
hsetnx key field val = Edis $ Redis.hsetnx (encodeKey key) (encodeKey field) (encode val)

--------------------------------------------------------------------------------
--  List
--------------------------------------------------------------------------------

blpop :: (KnownSymbol s, Serialize x, ListOf x ~ FromJust (Get xs s))
        => Proxy s -> Integer
        -> Edis xs xs (Either Reply (Maybe (ByteString, x)))
blpop key timeout = Edis $ Redis.blpop [encodeKey key] timeout >>= decodeBPOP

brpop :: (KnownSymbol s, Serialize x, ListOf x ~ FromJust (Get xs s))
        => Proxy s -> Integer
        -> Edis xs xs (Either Reply (Maybe (ByteString, x)))
brpop key timeout = Edis $ Redis.brpop [encodeKey key] timeout >>= decodeBPOP

brpoplpush :: (KnownSymbol s, KnownSymbol t, Serialize x
        , ListOf x ~ FromJust (Get xs s), ListOrNX xs s)
        => Proxy s -> Proxy t -> Integer
        -> Edis xs (Set xs s (ListOf x)) (Either Reply (Maybe x))
brpoplpush key dest timeout = Edis $ Redis.brpoplpush (encodeKey key) (encodeKey dest) timeout >>= decodeAsMaybe

lindex :: (KnownSymbol s, Serialize x, ListOf x ~ FromJust (Get xs s))
        => Proxy s -> Integer
        -> Edis xs xs (Either Reply (Maybe x))
lindex key index = Edis $ Redis.lindex (encodeKey key) index >>= decodeAsMaybe

linsertBefore :: (KnownSymbol s, Serialize x, ListOrNX xs s)
        => Proxy s -> x -> x
        -> Edis xs xs (Either Reply Integer)
linsertBefore key pivot val = Edis $ Redis.linsertBefore (encodeKey key) (encode pivot) (encode val)

linsertAfter :: (KnownSymbol s, Serialize x, ListOrNX xs s)
        => Proxy s -> x -> x
        -> Edis xs xs (Either Reply Integer)
linsertAfter key pivot val = Edis $ Redis.linsertAfter (encodeKey key) (encode pivot) (encode val)

llen :: (KnownSymbol s, ListOrNX xs s)
        => Proxy s
        -> Edis xs xs (Either Reply Integer)
llen key = Edis $ Redis.llen (encodeKey key)

lpop :: (KnownSymbol s, Serialize x, Just (ListOf x) ~ Get xs s)
        => Proxy s
        -> Edis xs xs (Either Reply (Maybe x))
lpop key = Edis $ Redis.lpop (encodeKey key) >>= decodeAsMaybe

lpush :: (KnownSymbol s, Serialize x, ListOrNX xs s)
      => Proxy s -> x
      -> Edis xs (Set xs s (ListOf x)) (Either Reply Integer)
lpush key val = Edis $ Redis.lpush (encodeKey key) [encode val]

-- no operation will be performed when key does not yet exist.
lpushx :: (KnownSymbol s, Serialize x, Just (ListOf x) ~ Get xs s)
        => Proxy s -> x
        -> Edis xs xs (Either Reply Integer)
lpushx key val = Edis $ Redis.lpushx (encodeKey key) (encode val)

lrange :: (KnownSymbol s, Serialize x, ListOrNX xs s)
     => Proxy s -> Integer -> Integer
     -> Edis xs xs (Either Reply [x])
lrange key start stop = Edis $ Redis.lrange (encodeKey key) start stop >>= decodeAsList

lrem :: (KnownSymbol s, Serialize x, ListOrNX xs s)
      => Proxy s -> Integer -> x
      -> Edis xs xs (Either Reply Integer)
lrem key count val = Edis $ Redis.lrem (encodeKey key) count (encode val)

lset :: (KnownSymbol s, Serialize x, IsList (FromJust (Get xs s)) ~ True)
      => Proxy s -> Integer -> x
      -> Edis xs xs (Either Reply Status)
lset key index val = Edis $ Redis.lset (encodeKey key) index (encode val)

ltrim :: (KnownSymbol s, ListOrNX xs s)
      =>  Proxy s -> Integer -> Integer
      -> Edis xs xs (Either Reply Status)
ltrim key start stop = Edis $ Redis.ltrim (encodeKey key) start stop

rpop :: (KnownSymbol s, Serialize x, Just (ListOf x) ~ Get xs s)
        => Proxy s
        -> Edis xs xs (Either Reply (Maybe x))
rpop key = Edis $ Redis.rpop (encodeKey key) >>= decodeAsMaybe

-- 1. source and target must be List or does not yet exists
rpoplpush :: (KnownSymbol s, KnownSymbol t, Serialize x
        , ListOrNX xs s , ListOrNX xs t)
        => Proxy s -> Proxy t
        -> Edis xs (If (IsList (FromJust (Get xs s)))
                    (Set xs s (ListOf x))
                    xs) (Either Reply (Maybe x))
rpoplpush key dest = Edis $ Redis.rpoplpush (encodeKey key) (encodeKey dest) >>= decodeAsMaybe

rpush :: (KnownSymbol s, Serialize x, ListOrNX xs s)
      => Proxy s -> x
      -> Edis xs (Set xs s (ListOf x)) (Either Reply Integer)
rpush key val = Edis $ Redis.rpush (encodeKey key) [encode val]

-- no operation will be performed when key does not yet exist.
rpushx :: (KnownSymbol s, Serialize x, Just (ListOf x) ~ Get xs s)
        => Proxy s -> x
        -> Edis xs xs (Either Reply Integer)
rpushx key val = Edis $ Redis.rpushx (encodeKey key) (encode val)

--------------------------------------------------------------------------------
--  Sets
--------------------------------------------------------------------------------

-- sadd :: (KnownSymbol s, Serialize x, SetOrNX xs s)
--         => Proxy s -> x
--         -> Edis xs (If (Member xs s) xs (Set xs s (SetOf x))) (Either Reply Integer)
-- sadd key val = Edis $ Redis.sadd (encodeKey key) [encode val]
--
-- scard :: (KnownSymbol s, SetOrNX xs s)
--         => Proxy s
--         -> Edis xs xs (Either Reply Integer)
-- scard key = Edis $ Redis.scard (encodeKey key)
--
-- sdiff :: (KnownSymbol s, KnownSymbol t, Serialize x
--         , Just (SetOf x) ~ Get xs s  -- must exist
--         , SetOrNX xs s)
--         => Proxy s -> Proxy t
--         -> Edis xs xs (Either Reply [x])
-- sdiff key key' = Edis $ Redis.sdiff [encodeKey key, encodeKey key'] >>= decodeAsList

-- sdiffstore :: (KnownSymbol s, KnownSymbol t, SetOrNX xs s, SetOrNX xs t)
--         => Proxy s -> Proxy t
--         -> Edis xs xs (Either Reply Integer)
-- sdiffstore dest key = Edis $ Redis.sdiffstore (encodeKey dest) [encodeKey key]
--
-- srem :: (Member xs s ~ True, FromJust (Get xs s) ~ SetOf x, Serialize x, KnownSymbol s)
--      => Proxy s -> x -> Edis xs xs (Either Reply Integer)
-- srem key val = Edis $ Redis.srem (encode $ symbolVal key) [encode val]
--
-- smembers :: (Member xs s ~ True, FromJust (Get xs s) ~ SetOf x, Serialize x, KnownSymbol s)
--          => Proxy s -> Edis xs xs  (Either Reply [x])
-- smembers key = Edis $ Redis.smembers (encode $ symbolVal key) >>= decodeAsList

--------------------------------------------------------------------------------
--  Connection
--------------------------------------------------------------------------------

auth :: ByteString -> Edis xs xs (Either Reply Status)
auth = Edis . Redis.auth

echo :: ByteString -> Edis xs xs (Either Reply ByteString)
echo = Edis . Redis.echo

ping :: Edis xs xs (Either Reply Status)
ping = Edis Redis.ping

quit :: Edis xs xs (Either Reply Status)
quit = Edis Redis.quit

select :: Integer -> Edis xs xs (Either Reply Status)
select = Edis . Redis.select

--------------------------------------------------------------------------------
--  Keys
--------------------------------------------------------------------------------

del :: KnownSymbol s => Proxy s -> Edis xs (Del xs s) (Either Reply Integer)
del key = Edis $ Redis.del [encodeKey key]

dump :: ByteString -> Edis xs xs (Either Reply ByteString)
dump key = Edis $ Redis.dump key

exists :: ByteString -> Edis xs xs (Either Reply Bool)
exists key = Edis $ Redis.exists key

expire :: ByteString -> Integer -> Edis xs xs (Either Reply Bool)
expire key n = Edis $ Redis.expire key n

expireat :: ByteString -> Integer -> Edis xs xs (Either Reply Bool)
expireat key n = Edis $ Redis.expireat key n

keys :: ByteString -> Edis xs xs (Either Reply [ByteString])
keys pattern = Edis $ Redis.keys pattern

migrate :: ByteString -> ByteString -> ByteString -> Integer -> Integer -> Edis xs xs (Either Reply Status)
migrate host port key destinationDb timeout = Edis $ Redis.migrate host port key destinationDb timeout

move :: ByteString -> Integer -> Edis xs xs (Either Reply Bool)
move key db = Edis $ Redis.move key db

objectRefcount :: ByteString -> Edis xs xs (Either Reply Integer)
objectRefcount key = Edis $ Redis.objectRefcount key

objectEncoding :: ByteString -> Edis xs xs (Either Reply ByteString)
objectEncoding key = Edis $ Redis.objectEncoding key

objectIdletime :: ByteString -> Edis xs xs (Either Reply Integer)
objectIdletime key = Edis $ Redis.objectIdletime key

persist :: ByteString -> Edis xs xs (Either Reply Bool)
persist key = Edis $ Redis.persist key

pexpire :: ByteString -> Integer -> Edis xs xs (Either Reply Bool)
pexpire key n = Edis $ Redis.pexpire key n

pexpireat :: ByteString -> Integer -> Edis xs xs (Either Reply Bool)
pexpireat key n = Edis $ Redis.pexpireat key n

pttl :: ByteString -> Edis xs xs (Either Reply Integer)
pttl key = Edis $ Redis.pttl key

randomkey :: Edis xs xs (Either Reply (Maybe ByteString))
randomkey = Edis $ Redis.randomkey

-- 1. the old key must exist
-- 2. the old key and the new key must be different
rename :: (KnownSymbol s, KnownSymbol t, Member xs s ~ True, Get xs s ~ Just x, (s == t) ~ False)
        => Proxy s -> Proxy t -> Edis xs (Set (Del xs s) t x) (Either Reply Status)
rename key key' = Edis $ Redis.rename (encodeKey key) (encodeKey key')



-- 1. the old key must exist
-- 2. the old key and the new key must be different
-- 3. if the new key exists
--      then nothing happens
--      else the new key is removed
renamenx :: (KnownSymbol s, KnownSymbol t, Member xs s ~ True, Get xs s ~ Just x, (s == t) ~ False)
        => Proxy s -> Proxy t -> Edis xs (If (Member xs t) xs (Del xs s)) (Either Reply Bool)
renamenx key key' = Edis $ Redis.renamenx (encodeKey key) (encodeKey key')

restore :: ByteString -> Integer -> ByteString -> Edis xs xs (Either Reply Status)
restore key n val = Edis $ Redis.restore key n val

-- must be a List, a Set or a Sorted Set
sort :: (KnownSymbol s, Serialize x, Member xs s ~ True
        , (Get xs s == Just (ListOf x) || Get xs s == Just (SetOf x) || Get xs s == Just (ZSetOf x)) ~ True)
        => Proxy s -> SortOpts -> Edis xs xs (Either Reply [x])
sort key opt = Edis $ Redis.sort (encodeKey key) opt >>= decodeAsList

sortStore :: (KnownSymbol s, KnownSymbol t, Member xs s ~ True, FromJust (Get xs s) ~ x
        , (IsList (FromJust (Get xs s)) || IsSet (FromJust (Get xs s)) || IsZSet (FromJust (Get xs s))) ~ True)
        => Proxy s -> Proxy t -> SortOpts -> Edis xs (Set xs s x) (Either Reply Integer)
sortStore key dest opt = Edis $ Redis.sortStore (encodeKey key) (encodeKey dest) opt

ttl :: ByteString -> Edis xs xs (Either Reply Integer)
ttl key = Edis $ Redis.ttl key

getType :: ByteString -> Edis xs xs (Either Reply RedisType)
getType key = Edis $ Redis.getType key

--------------------------------------------------------------------------------
--  String
--------------------------------------------------------------------------------

append :: (KnownSymbol s, Serialize x, StringOrNX xs s)
        => Proxy s -> x -> Edis xs (Set xs s (StringOf ByteString)) (Either Reply Integer)
append key val = Edis $ Redis.append (encodeKey key) (encode val)

bitcount :: (KnownSymbol s, StringOrNX xs s)
        => Proxy s -> Edis xs xs (Either Reply Integer)
bitcount key = Edis $ Redis.bitcount (encodeKey key)

bitcountRange :: (KnownSymbol s, StringOrNX xs s)
        => Proxy s -> Integer -> Integer -> Edis xs xs (Either Reply Integer)
bitcountRange key m n = Edis $ Redis.bitcountRange (encodeKey key) m n

--  key must be String or non-existent
bitopAnd :: (KnownSymbol s, KnownSymbol t, StringOrNX xs s)
        => Proxy t -> Proxy s -> Edis xs (Set xs s (StringOf ByteString)) (Either Reply Integer)
bitopAnd dest key = Edis $ Redis.bitopAnd (encodeKey dest) [encodeKey key]

bitopOr :: (KnownSymbol s, KnownSymbol t, StringOrNX xs s)
        => Proxy t -> Proxy s -> Edis xs (Set xs s (StringOf ByteString)) (Either Reply Integer)
bitopOr dest key = Edis $ Redis.bitopOr (encodeKey dest) [encodeKey key]

bitopXor :: (KnownSymbol s, KnownSymbol t, StringOrNX xs s)
        => Proxy t -> Proxy s -> Edis xs (Set xs s (StringOf ByteString)) (Either Reply Integer)
bitopXor dest key = Edis $ Redis.bitopXor (encodeKey dest) [encodeKey key]

bitopNot :: (KnownSymbol s, KnownSymbol t, StringOrNX xs s)
        => Proxy t -> Proxy s -> Edis xs (Set xs s (StringOf ByteString)) (Either Reply Integer)
bitopNot dest key = Edis $ Redis.bitopNot (encodeKey dest) (encodeKey key)

decr :: (KnownSymbol s, StringOfIntegerOrNX xs s)
        => Proxy s -> Edis xs (Set xs s (StringOf Integer)) (Either Reply Integer)
decr key = Edis $ Redis.decr (encodeKey key)

decrby :: (KnownSymbol s, StringOfIntegerOrNX xs s)
        => Proxy s -> Integer -> Edis xs (Set xs s (StringOf Integer)) (Either Reply Integer)
decrby key n = Edis $ Redis.decrby (encodeKey key) n

get :: (KnownSymbol s, Serialize x, StringOf x ~ FromJust (Get xs s))
        => Proxy s -> Edis xs xs (Either Reply (Maybe x))
get key = Edis $ Redis.get (encodeKey key) >>= decodeAsMaybe

getbit :: (KnownSymbol s, StringOrNX xs s)
        => Proxy s -> Integer -> Edis xs xs (Either Reply Integer)
getbit key n = Edis $ Redis.getbit (encodeKey key) n

getrange :: (KnownSymbol s, Serialize x, StringOf x ~ FromJust (Get xs s))
        => Proxy s -> Integer -> Integer -> Edis xs xs (Either Reply x)
getrange key n m = Edis $ Redis.getrange (encodeKey key) n m >>= decodeAsAnything

getset :: (KnownSymbol s, Serialize x, Serialize y, StringOf y ~ FromJust (Get xs s))
        => Proxy s -> x -> Edis xs xs (Either Reply (Maybe y))
getset key val = Edis $ Redis.getset (encodeKey key) (encode val) >>= decodeAsMaybe

incr :: (KnownSymbol s, StringOfIntegerOrNX xs s)
        => Proxy s -> Edis xs xs (Either Reply Integer)
incr key = Edis $ Redis.incr (encodeKey key)

incrby :: (KnownSymbol s, StringOfIntegerOrNX xs s)
        => Proxy s -> Integer -> Edis xs xs (Either Reply Integer)
incrby key n = Edis $ Redis.incrby (encodeKey key) n

incrbyfloat :: (KnownSymbol s, StringOfDoubleOrNX xs s)
        => Proxy s -> Double -> Edis xs xs (Either Reply Double)
incrbyfloat key n = Edis $ Redis.incrbyfloat (encodeKey key) n

psetex :: (KnownSymbol s, Serialize x)
        => Proxy s -> Integer -> x -> Edis xs (Set xs s (StringOf x)) (Either Reply Status)
psetex key n val = Edis $ Redis.psetex (encodeKey key) n (encode val)

set :: (KnownSymbol s, Serialize x)
        => Proxy s -> x -> Edis xs (Set xs s (StringOf x)) (Either Reply Status)
set key val = Edis $ Redis.set (encodeKey key) (encode val)

setbit :: (KnownSymbol s, Serialize x, StringOrNX xs s)
        => Proxy s -> Integer -> x -> Edis xs (Set xs s (StringOf x)) (Either Reply Integer)
setbit key n val = Edis $ Redis.setbit (encodeKey key) n (encode val)

setex :: (KnownSymbol s, Serialize x)
        => Proxy s -> Integer -> x -> Edis xs (Set xs s (StringOf x)) (Either Reply Status)
setex key n val = Edis $ Redis.setex (encodeKey key) n (encode val)

setnx :: (KnownSymbol s, Serialize x)
        => Proxy s -> x -> Edis xs (If (Member xs s) xs (Set xs s (StringOf x))) (Either Reply Bool)
setnx key val = Edis $ Redis.setnx (encodeKey key) (encode val)

setrange :: (KnownSymbol s, Serialize x, StringOrNX xs s)
        => Proxy s -> Integer -> x -> Edis xs (Set xs s (StringOf x)) (Either Reply Integer)
setrange key n val = Edis $ Redis.setrange (encodeKey key) n (encode val)

strlen :: (KnownSymbol s, StringOrNX xs s)
        => Proxy s -> Edis xs xs (Either Reply Integer)
strlen key = Edis $ Redis.strlen (encodeKey key)

--------------------------------------------------------------------------------
--  Helper functions
--------------------------------------------------------------------------------

decodeAsAnything :: (Serialize x) => Either Reply ByteString -> Redis (Either Reply x)
decodeAsAnything (Left replyErr) = return $ Left replyErr
decodeAsAnything (Right str) = case decode str of
        Left decodeErr -> return $ Left (Error $ encode decodeErr)
        Right val      -> return $ Right val

decodeAsMaybe :: (Serialize x) => Either Reply (Maybe ByteString) -> Redis (Either Reply (Maybe x))
decodeAsMaybe (Left replyErr) = return $ Left replyErr
decodeAsMaybe (Right Nothing) = return $ Right Nothing
decodeAsMaybe (Right (Just str)) = case decode str of
        Left decodeErr -> return $ Left (Error $ encode decodeErr)
        Right val      -> return $ Right (Just val)

decodeBPOP :: Serialize x => Either Reply (Maybe (ByteString, ByteString)) -> Redis (Either Reply (Maybe (ByteString, x)))
decodeBPOP (Left replyError)         = return $ Left replyError
decodeBPOP (Right Nothing)           = return $ Right Nothing
decodeBPOP (Right (Just (key, raw))) = case decode raw of
    Left decodeError -> return $ Left (Error $ encode decodeError)
    Right value      -> return $ Right (Just (key, value))

decodeAsList :: (Serialize x) => Either Reply [ByteString] -> Redis (Either Reply [x])
decodeAsList (Left replyErr) = return $ Left replyErr
decodeAsList (Right strs) = case mapM decode strs of
    Left decodeErr -> return $ Left (Error $ encode decodeErr)
    Right vals     -> return $ Right vals

fromRight :: Either a b -> b
fromRight (Left e) = error "Left val"
fromRight (Right e) = e

fromMaybeResult :: Either Reply (Maybe x) -> x
fromMaybeResult = fromJust . fromRight
