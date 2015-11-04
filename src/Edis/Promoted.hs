{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies, TypeOperators
    , OverloadedStrings, MultiParamTypeClasses #-}

module Edis.Promoted where

import Edis.Type
-- import Edis.Dict
import Edis.Serialize

import Data.Maybe (fromJust)
import Data.Bifunctor (first)
import Data.Type.Bool
import Data.Type.Equality
import Database.Redis as Redis
import Data.Proxy
import GHC.TypeLits

--------------------------------------------------------------------------------
--  Helper functions
--------------------------------------------------------------------------------

encodeKey :: KnownSymbol s => Proxy s -> ByteString
encodeKey = enc . symbolVal

start :: P '[] '[] ()
start = P (return ())

--------------------------------------------------------------------------------
--  Declaration
--------------------------------------------------------------------------------

declare :: (KnownSymbol s, Member xs s ~ False)
        => Proxy s -> Proxy x -> P xs (Set xs s x) ()
declare s x = P $ return ()

renounce :: (KnownSymbol s, Member xs s ~ True)
        => Proxy s -> P xs (Del xs s) ()
renounce s = P $ return ()

--------------------------------------------------------------------------------
--  Hashes
--------------------------------------------------------------------------------

hdel :: (KnownSymbol k, KnownSymbol f
        , (Not (Member xs k) || IsHash (FromJust (Get xs k))) ~ True)
        => Proxy k -> Proxy f -> P xs (DelHash xs k f) (Either Reply Integer)
hdel key field = P $ Redis.hdel (encodeKey key) [encodeKey field]

hexists :: (KnownSymbol k, KnownSymbol f
        , (Not (Member xs k) || IsHash (FromJust (Get xs k))) ~ True)
        => Proxy k -> Proxy f -> P xs xs (Either Reply Bool)
hexists key field = P $ Redis.hexists (encodeKey key) (encodeKey field)

hget :: (KnownSymbol k, KnownSymbol f, Value x, Just x ~ HGet (FromJust (Get xs k)) f
        , (Not (Member xs k) || IsHash (FromJust (Get xs k))) ~ True)
        => Proxy k -> Proxy f -> P xs xs (Either Reply (Maybe x))
hget key field = P $ Redis.hget (encodeKey key) (encodeKey field) >>= decodeAsMaybe

hincrby :: (KnownSymbol k, KnownSymbol f
        , (Not (Member xs k) || GetHash xs k f == Just Integer) ~ True)
        => Proxy k -> Proxy f -> Integer -> P xs (SetHash xs k f Integer) (Either Reply Integer)
hincrby key field n = P $ Redis.hincrby (encodeKey key) (encodeKey field) n

hincrbyfloat :: (KnownSymbol k, KnownSymbol f
        , (Not (Member xs k) || GetHash xs k f == Just Double) ~ True)
        => Proxy k -> Proxy f -> Double -> P xs (SetHash xs k f Double) (Either Reply Double)
hincrbyfloat key field n = P $ Redis.hincrbyfloat (encodeKey key) (encodeKey field) n

hkeys :: (KnownSymbol k, (Not (Member xs k) || IsHash (FromJust (Get xs k))) ~ True)
        => Proxy k -> P xs xs (Either Reply [ByteString])
hkeys key = P $ Redis.hkeys (encodeKey key)

hlen :: (KnownSymbol k, (Not (Member xs k) || IsHash (FromJust (Get xs k))) ~ True)
        => Proxy k -> P xs xs (Either Reply Integer)
hlen key = P $ Redis.hlen (encodeKey key)

hset :: (KnownSymbol k, KnownSymbol f, Value x
        , (Not (Member xs k) || IsHash (FromJust (Get xs k))) ~ True)
        => Proxy k -> Proxy f -> x -> P xs (SetHash xs k f x) (Either Reply Bool)
hset key field val = P $ Redis.hset (encodeKey key) (encodeKey field) (enc val)

hsetnx :: (KnownSymbol k, KnownSymbol f, Value x
        , (Not (Member xs k) || IsHash (FromJust (Get xs k))) ~ True)
        => Proxy k -> Proxy f -> x
        -> P xs (If (MemHash xs k f) xs (SetHash xs k f x)) (Either Reply Bool)
hsetnx key field val = P $ Redis.hsetnx (encodeKey key) (encodeKey field) (enc val)

--------------------------------------------------------------------------------
--  List
--------------------------------------------------------------------------------

blpop :: (KnownSymbol s, Value x, ListK x ~ FromJust (Get xs s))
        => Proxy s -> Integer -> P xs xs (Either Reply (Maybe (ByteString, x)))
blpop key timeout = P $ Redis.blpop [encodeKey key] timeout >>= decodeBPOP

brpop :: (KnownSymbol s, Value x, ListK x ~ FromJust (Get xs s))
        => Proxy s -> Integer -> P xs xs (Either Reply (Maybe (ByteString, x)))
brpop key timeout = P $ Redis.brpop [encodeKey key] timeout >>= decodeBPOP

-- 1. the source and the target has to be a List
-- 2. the source will be deleted once it's empty
brpoplpush :: (KnownSymbol s, KnownSymbol t, Value x
        , ListK x ~ FromJust (Get xs s), IsList (FromJust (Get xs t)) ~ True)
        => Proxy s -> Proxy t -> Integer -> P xs (Set xs s (ListK x)) (Either Reply (Maybe x))
brpoplpush key dest timeout = P $ Redis.brpoplpush (encodeKey key) (encodeKey dest) timeout >>= decodeAsMaybe

lindex :: (KnownSymbol s, Value x, ListK x ~ FromJust (Get xs s))
        => Proxy s -> Integer -> P xs xs (Either Reply (Maybe x))
lindex key index = P $ Redis.lindex (encodeKey key) index >>= decodeAsMaybe

-- remains uneffected if doesn't exists at all
linsertBefore :: (KnownSymbol s, Value x
        , (Not (Member xs s) || IsList (FromJust (Get xs s))) ~ True)
        => Proxy s -> x -> x -> P xs xs (Either Reply Integer)
linsertBefore key pivot val = P $ Redis.linsertBefore (encodeKey key) (enc pivot) (enc val)

linsertAfter :: (KnownSymbol s, Value x
        , (Not (Member xs s) || IsList (FromJust (Get xs s))) ~ True)
        => Proxy s -> x -> x -> P xs xs (Either Reply Integer)
linsertAfter key pivot val = P $ Redis.linsertAfter (encodeKey key) (enc pivot) (enc val)

llen :: (KnownSymbol s, IsList (FromJust (Get xs s)) ~ True)
        => Proxy s -> P xs xs (Either Reply Integer)
llen key = P $ Redis.llen (encodeKey key)

lpop :: (KnownSymbol s, Value x, Just (ListK x) ~ Get xs s)
        => Proxy s -> P xs xs (Either Reply (Maybe x))
lpop key = P $ Redis.lpop (encodeKey key) >>= decodeAsMaybe

lpush :: (KnownSymbol s, Value x
        , (Not (Member xs s) || IsList (FromJust (Get xs s))) ~ True)
      => Proxy s -> x -> P xs (Set xs s (ListK x)) (Either Reply Integer)
lpush key val = P $ Redis.lpush (encodeKey key) [enc val]

-- no operation will be performed when key does not yet exist.
lpushx :: (KnownSymbol s, Value x, Just (ListK x) ~ Get xs s)
        => Proxy s -> x -> P xs xs (Either Reply Integer)
lpushx key val = P $ Redis.lpushx (encodeKey key) (enc val)

lrange :: (KnownSymbol s, Value x
        , (Not (Member xs s) || IsList (FromJust (Get xs s))) ~ True)
     => Proxy s -> Integer -> Integer -> P xs xs (Either Reply [x])
lrange key start stop = P $ Redis.lrange (encodeKey key) start stop >>= decodeAsList

lrem :: (KnownSymbol s, Value x
        , (Not (Member xs s) || IsList (FromJust (Get xs s))) ~ True)
      => Proxy s -> Integer -> x -> P xs xs (Either Reply Integer)
lrem key count val = P $ Redis.lrem (encodeKey key) count (enc val)

-- error if key does not yet exist
-- error if out of index
lset :: (KnownSymbol s, Value x, IsList (FromJust (Get xs s)) ~ True)
      => Proxy s -> Integer -> x -> P xs xs (Either Reply Status)
lset key index val = P $ Redis.lset (encodeKey key) index (enc val)

ltrim :: (KnownSymbol s
        , (Not (Member xs s) || IsList (FromJust (Get xs s))) ~ True)
      =>  Proxy s -> Integer -> Integer -> P xs xs (Either Reply Status)
ltrim key start stop = P $ Redis.ltrim (encodeKey key) start stop

rpop :: (KnownSymbol s, Value x, Just (ListK x) ~ Get xs s)
        => Proxy s -> P xs xs (Either Reply (Maybe x))
rpop key = P $ Redis.rpop (encodeKey key) >>= decodeAsMaybe

-- 1. source and target must be List or does not yet exists
rpoplpush :: (KnownSymbol s, KnownSymbol t, Value x
        , (Not (Member xs s) || IsList (FromJust (Get xs s))) ~ True
        , (Not (Member xs t) || IsList (FromJust (Get xs t))) ~ True)
        => Proxy s -> Proxy t -> P xs (If (IsList (FromJust (Get xs s))) (Set xs s (ListK x)) xs) (Either Reply (Maybe x))
rpoplpush key dest = P $ Redis.rpoplpush (encodeKey key) (encodeKey dest) >>= decodeAsMaybe

rpush :: (KnownSymbol s, Value x
        , (Not (Member xs s) || IsList (FromJust (Get xs s))) ~ True)
      => Proxy s -> x -> P xs (Set xs s (ListK x)) (Either Reply Integer)
rpush key val = P $ Redis.rpush (encodeKey key) [enc val]

-- no operation will be performed when key does not yet exist.
rpushx :: (KnownSymbol s, Value x, Just (ListK x) ~ Get xs s)
        => Proxy s -> x -> P xs xs (Either Reply Integer)
rpushx key val = P $ Redis.rpushx (encodeKey key) (enc val)



--------------------------------------------------------------------------------
--  Connection
--------------------------------------------------------------------------------

auth :: ByteString -> P xs xs (Either Reply Status)
auth = P . Redis.auth

echo :: ByteString -> P xs xs (Either Reply ByteString)
echo = P . Redis.echo

ping :: P xs xs (Either Reply Status)
ping = P Redis.ping

quit :: P xs xs (Either Reply Status)
quit = P Redis.quit

select :: Integer -> P xs xs (Either Reply Status)
select = P . Redis.select

--------------------------------------------------------------------------------
--  Keys
--------------------------------------------------------------------------------

del :: (KnownSymbol s)
        => Proxy s -> P xs (Del xs s) (Either Reply Integer)
del key = P $ Redis.del [encodeKey key]

dump :: ByteString -> P xs xs (Either Reply ByteString)
dump key = P $ Redis.dump key

exists :: ByteString -> P xs xs (Either Reply Bool)
exists key = P $ Redis.exists key

expire :: ByteString -> Integer -> P xs xs (Either Reply Bool)
expire key n = P $ Redis.expire key n

expireat :: ByteString -> Integer -> P xs xs (Either Reply Bool)
expireat key n = P $ Redis.expireat key n

keys :: ByteString -> P xs xs (Either Reply [ByteString])
keys pattern = P $ Redis.keys pattern

migrate :: ByteString -> ByteString -> ByteString -> Integer -> Integer -> P xs xs (Either Reply Status)
migrate host port key destinationDb timeout = P $ Redis.migrate host port key destinationDb timeout

move :: ByteString -> Integer -> P xs xs (Either Reply Bool)
move key db = P $ Redis.move key db

objectRefcount :: ByteString -> P xs xs (Either Reply Integer)
objectRefcount key = P $ Redis.objectRefcount key

objectEncoding :: ByteString -> P xs xs (Either Reply ByteString)
objectEncoding key = P $ Redis.objectEncoding key

objectIdletime :: ByteString -> P xs xs (Either Reply Integer)
objectIdletime key = P $ Redis.objectIdletime key

persist :: ByteString -> P xs xs (Either Reply Bool)
persist key = P $ Redis.persist key

pexpire :: ByteString -> Integer -> P xs xs (Either Reply Bool)
pexpire key n = P $ Redis.pexpire key n

pexpireat :: ByteString -> Integer -> P xs xs (Either Reply Bool)
pexpireat key n = P $ Redis.pexpireat key n

pttl :: ByteString -> P xs xs (Either Reply Integer)
pttl key = P $ Redis.pttl key

randomkey :: P xs xs (Either Reply (Maybe ByteString))
randomkey = P $ Redis.randomkey

-- 1. the old key must exist
-- 2. the old key and the new key must be different
rename :: (KnownSymbol s, KnownSymbol t, Member xs s ~ True, Get xs s ~ Just x, (s == t) ~ False)
        => Proxy s -> Proxy t -> P xs (Set (Del xs s) t x) (Either Reply Status)
rename key key' = P $ Redis.rename (encodeKey key) (encodeKey key')



-- 1. the old key must exist
-- 2. the old key and the new key must be different
-- 3. if the new key exists
--      then nothing happens
--      else the new key is removed
renamenx :: (KnownSymbol s, KnownSymbol t, Member xs s ~ True, Get xs s ~ Just x, (s == t) ~ False)
        => Proxy s -> Proxy t -> P xs (If (Member xs t) xs (Del xs s)) (Either Reply Bool)
renamenx key key' = P $ Redis.renamenx (encodeKey key) (encodeKey key')

restore :: ByteString -> Integer -> ByteString -> P xs xs (Either Reply Status)
restore key n val = P $ Redis.restore key n val

-- must be a List, a Set or a Sorted Set
sort :: (KnownSymbol s, Value x, Member xs s ~ True, (Get xs s == Just (ListK x) || Get xs s == Just (SetK x) || Get xs s == Just (ZSetK x)) ~ True)
        => Proxy s -> SortOpts -> P xs xs (Either Reply [x])
sort key opt = P $ Redis.sort (encodeKey key) opt >>= decodeAsList

sortStore :: (KnownSymbol s, KnownSymbol t, Member xs s ~ True, FromJust (Get xs s) ~ x
        , (IsList (FromJust (Get xs s)) || IsSet (FromJust (Get xs s)) || IsZSet (FromJust (Get xs s))) ~ True)
        => Proxy s -> Proxy t -> SortOpts -> P xs (Set xs s x) (Either Reply Integer)
sortStore key dest opt = P $ Redis.sortStore (encodeKey key) (encodeKey dest) opt

ttl :: ByteString -> P xs xs (Either Reply Integer)
ttl key = P $ Redis.ttl key

getType :: ByteString -> P xs xs (Either Reply RedisType)
getType key = P $ Redis.getType key

--------------------------------------------------------------------------------
--  String
--------------------------------------------------------------------------------

append :: (Value x, KnownSymbol s)
        => Proxy s -> x -> P xs (Set xs s ByteString) (Either Reply Integer)
append key val = P $ Redis.append (encodeKey key) (enc val)

bitcount :: ByteString -> P xs xs (Either Reply Integer)
bitcount key = P $ Redis.bitcount key

bitcountRange :: ByteString -> Integer -> Integer -> P xs xs (Either Reply Integer)
bitcountRange key m n = P $ Redis.bitcountRange key m n

bitopAnd :: ByteString -> [ByteString] -> P xs xs (Either Reply Integer)
bitopAnd key keys = P $ Redis.bitopAnd key keys

bitopOr :: ByteString -> [ByteString] -> P xs xs (Either Reply Integer)
bitopOr key keys = P $ Redis.bitopOr key keys

bitopXor :: ByteString -> [ByteString] -> P xs xs (Either Reply Integer)
bitopXor key keys = P $ Redis.bitopXor key keys

bitopNot :: ByteString -> ByteString -> P xs xs (Either Reply Integer)
bitopNot keyA keyB = P $ Redis.bitopNot keyA keyB

decr :: (KnownSymbol s, (Not (Member xs s) || Get xs s == Just Integer) ~ True)
        => Proxy s -> P xs (Set xs s Integer) (Either Reply Integer)
decr key = P $ Redis.decr (encodeKey key)

decrby :: (KnownSymbol s, (Not (Member xs s) || Get xs s == Just Integer) ~ True)
        => Proxy s -> Integer -> P xs (Set xs s Integer) (Either Reply Integer)
decrby key n = P $ Redis.decrby (encodeKey key) n

get :: (KnownSymbol s, Value x, Member xs s ~ True, x ~ FromJust (Get xs s))
        => Proxy s -> P xs xs (Either Reply (Maybe x))
get key = P $ Redis.get (encodeKey key) >>= decodeAsMaybe

getbit :: ByteString -> Integer -> P xs xs (Either Reply Integer)
getbit key n = P $ Redis.getbit key n

getrange :: (KnownSymbol s, Value x, Member xs s ~ True, x ~ FromJust (Get xs s))
        => Proxy s -> Integer -> Integer -> P xs xs (Either Reply x)
getrange key n m = P $ Redis.getrange (encodeKey key) n m >>= decodeAsAnything

getset :: (KnownSymbol s, Value x, Value y, Member xs s ~ True, y ~ FromJust (Get xs s))
        => Proxy s -> x -> P xs xs (Either Reply (Maybe y))
getset key val = P $ Redis.getset (encodeKey key) (enc val) >>= decodeAsMaybe

incr :: (KnownSymbol s, (Not (Member xs s) || Get xs s == Just Integer) ~ True)
        => Proxy s -> P xs xs (Either Reply Integer)
incr key = P $ Redis.incr (encodeKey key)

incrby :: (KnownSymbol s, (Not (Member xs s) || Get xs s == Just Integer) ~ True)
        => Proxy s -> Integer -> P xs xs (Either Reply Integer)
incrby key n = P $ Redis.incrby (encodeKey key) n

incrbyfloat :: (KnownSymbol s, (Not (Member xs s) || Get xs s == Just Double) ~ True)
        => Proxy s -> Double -> P xs xs (Either Reply Double)
incrbyfloat key n = P $ Redis.incrbyfloat (encodeKey key) n

psetex :: (KnownSymbol s, Value x)
        => Proxy s -> Integer -> x -> P xs (Set xs s x) (Either Reply Status)
psetex key n val = P $ Redis.psetex (encodeKey key) n (enc val)

set :: (Value x, KnownSymbol s)
        => Proxy s -> x -> P xs (Set xs s x) (Either Reply Status)
set key val = P $ Redis.set (encodeKey key) (enc val)

setbit :: (Value x, KnownSymbol s)
        => Proxy s -> Integer -> x -> P xs (Set xs s x) (Either Reply Integer)
setbit key n val = P $ Redis.setbit (encodeKey key) n (enc val)

setex :: (Value x, KnownSymbol s)
        => Proxy s -> Integer -> x -> P xs (Set xs s x) (Either Reply Status)
setex key n val = P $ Redis.setex (encodeKey key) n (enc val)

setnx :: (Value x, KnownSymbol s)
        => Proxy s -> x -> P xs (If (Member xs s) xs (Set xs s x)) (Either Reply Bool)
setnx key val = P $ Redis.setnx (encodeKey key) (enc val)

setrange :: (Value x, KnownSymbol s)
        => Proxy s -> Integer -> x -> P xs (Set xs s x) (Either Reply Integer)
setrange key n val = P $ Redis.setrange (encodeKey key) n (enc val)

strlen :: ByteString -> P xs xs (Either Reply Integer)
strlen key = P $ Redis.strlen key

--------------------------------------------------------------------------------
--  Set
--------------------------------------------------------------------------------

-- SADD s x xs = Member xs s ~ False || Get xs s ~ Just (SetK x)
type family SADD (s :: Symbol) (x :: *) (xs :: [ (Symbol, *) ]) :: Bool where
    SADD s x '[]                  = True
    SADD s x ('(s, SetK x) ': xs) = True
    SADD s x ('(s, y)      ': xs) = False
    SADD s x ('(t, y)      ': xs) = SADD s x xs

sadd :: (SADD s x xs ~ True, Value x, KnownSymbol s)
     => Proxy s -> x -> P xs (Set xs s (SetK x))  (Either Reply Integer)
sadd key val = P $ Redis.sadd (enc $ symbolVal key) [enc val]

srem :: (Member xs s ~ True, FromJust (Get xs s) ~ SetK x, Value x, KnownSymbol s)
     => Proxy s -> x -> P xs xs (Either Reply Integer)
srem key val = P $ Redis.srem (enc $ symbolVal key) [enc val]

scard :: (Member xs s ~ True, FromJust (Get xs s) ~ SetK x, Value x, KnownSymbol s)
      => Proxy s -> P xs xs  (Either Reply Integer)
scard key = P $ Redis.scard (enc $ symbolVal key)

smembers :: (Member xs s ~ True, FromJust (Get xs s) ~ SetK x, Value x, KnownSymbol s)
         => Proxy s -> P xs xs  (Either Reply [x])
smembers key = P $ Redis.smembers (enc $ symbolVal key) >>= decodeAsList

--------------------------------------------------------------------------------
--  Helper functions
--------------------------------------------------------------------------------


decodeAsAnything :: (Value x) => Either Reply ByteString -> Redis (Either Reply x)
decodeAsAnything (Left replyErr) = return $ Left replyErr
decodeAsAnything (Right str) = case dec str of
        Left decodeErr -> return $ Left (Error $ enc decodeErr)
        Right val      -> return $ Right val

decodeAsMaybe :: (Value x) => Either Reply (Maybe ByteString) -> Redis (Either Reply (Maybe x))
decodeAsMaybe (Left replyErr) = return $ Left replyErr
decodeAsMaybe (Right Nothing) = return $ Right Nothing
decodeAsMaybe (Right (Just str)) = case dec str of
        Left decodeErr -> return $ Left (Error $ enc decodeErr)
        Right val      -> return $ Right (Just val)

decodeBPOP :: Value x => Either Reply (Maybe (ByteString, ByteString)) -> Redis (Either Reply (Maybe (ByteString, x)))
decodeBPOP (Left replyError)         = return $ Left replyError
decodeBPOP (Right Nothing)           = return $ Right Nothing
decodeBPOP (Right (Just (key, raw))) = case dec raw of
    Left decodeError -> return $ Left (Error $ enc decodeError)
    Right value      -> return $ Right (Just (key, value))

decodeAsList :: (Value x) => Either Reply [ByteString] -> Redis (Either Reply [x])
decodeAsList (Left replyErr) = return $ Left replyErr
decodeAsList (Right strs) = case mapM dec strs of
    Left decodeErr -> return $ Left (Error $ enc decodeErr)
    Right vals     -> return $ Right vals

fromRight :: Either a b -> b
fromRight (Left e) = error "Left val"
fromRight (Right e) = e

fromMaybeResult :: Either Reply (Maybe x) -> x
fromMaybeResult = fromJust . fromRight
