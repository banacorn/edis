{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies, TypeOperators
    , OverloadedStrings, MultiParamTypeClasses #-}

module Edis.Promoted where

import Edis.Type
import Edis.Dict
import Edis.Serialize

import Data.Maybe (fromJust)
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

declare :: (Member s xs ~ False, KnownSymbol s)
    => Proxy s -> Proxy x -> P xs (Set s x xs) ()
declare s x = P $ return ()

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
        => Proxy s -> P xs (Del s xs) (Either Reply Integer)
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
rename :: (KnownSymbol s, KnownSymbol t, Member s xs ~ True, Get s xs ~ Just x, (s == t) ~ False)
        => Proxy s -> Proxy t -> P xs (Set t x (Del s xs)) (Either Reply Status)
rename key key' = P $ Redis.rename (encodeKey key) (encodeKey key')



-- 1. the old key must exist
-- 2. the old key and the new key must be different
-- 3. if the new key exists
--      then nothing happens
--      else the new key is removed
renamenx :: (KnownSymbol s, KnownSymbol t, Member s xs ~ True, Get s xs ~ Just x, (s == t) ~ False)
        => Proxy s -> Proxy t -> P xs (If (Member t xs) xs (Del s xs)) (Either Reply Bool)
renamenx key key' = P $ Redis.renamenx (encodeKey key) (encodeKey key')

restore :: ByteString -> Integer -> ByteString -> P xs xs (Either Reply Status)
restore key n val = P $ Redis.restore key n val

-- must be a List, a Set or a Sorted Set
sort :: (KnownSymbol s, Value x, Member s xs ~ True, (Get s xs == Just (ListK x) || Get s xs == Just (SetK x) || Get s xs == Just (ZSetK x)) ~ True)
        => Proxy s -> SortOpts -> P xs xs (Either Reply [x])
sort key opt = P $ Redis.sort (encodeKey key) opt >>= decodeAsList

sortStore :: (KnownSymbol s, KnownSymbol t, Member s xs ~ True, FromJust (Get s xs) ~ x
        , (IsList (FromJust (Get s xs)) || IsSet (FromJust (Get s xs)) || IsZSet (FromJust (Get s xs))) ~ True)
        => Proxy s -> Proxy t -> SortOpts -> P xs (Set s x xs) (Either Reply Integer)
sortStore key dest opt = P $ Redis.sortStore (encodeKey key) (encodeKey dest) opt

ttl :: ByteString -> P xs xs (Either Reply Integer)
ttl key = P $ Redis.ttl key

getType :: ByteString -> P xs xs (Either Reply RedisType)
getType key = P $ Redis.getType key

--------------------------------------------------------------------------------
--  String
--------------------------------------------------------------------------------

append :: (Value x, KnownSymbol s)
        => Proxy s -> x -> P xs (Set s ByteString xs) (Either Reply Integer)
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

decr :: (KnownSymbol s, (Not (Member s xs) || Get s xs == Just Integer) ~ True)
        => Proxy s -> P xs (Set s Integer xs) (Either Reply Integer)
decr key = P $ Redis.decr (encodeKey key)

decrby :: (KnownSymbol s, (Not (Member s xs) || Get s xs == Just Integer) ~ True)
        => Proxy s -> Integer -> P xs (Set s Integer xs) (Either Reply Integer)
decrby key n = P $ Redis.decrby (encodeKey key) n

get :: (KnownSymbol s, Value x, Member s xs ~ True, x ~ FromJust (Get s xs))
        => Proxy s -> P xs xs (Either Reply (Maybe x))
get key = P $ Redis.get (encodeKey key) >>= decodeAsMaybe

getbit :: ByteString -> Integer -> P xs xs (Either Reply Integer)
getbit key n = P $ Redis.getbit key n

getrange :: (KnownSymbol s, Value x, Member s xs ~ True, x ~ FromJust (Get s xs))
        => Proxy s -> Integer -> Integer -> P xs xs (Either Reply x)
getrange key n m = P $ Redis.getrange (encodeKey key) n m >>= decodeAsAnything

getset :: (KnownSymbol s, Value x, Value y, Member s xs ~ True, y ~ FromJust (Get s xs))
        => Proxy s -> x -> P xs xs (Either Reply (Maybe y))
getset key val = P $ Redis.getset (encodeKey key) (enc val) >>= decodeAsMaybe

incr :: (KnownSymbol s, (Not (Member s xs) || Get s xs == Just Integer) ~ True)
        => Proxy s -> P xs xs (Either Reply Integer)
incr key = P $ Redis.incr (encodeKey key)

incrby :: (KnownSymbol s, (Not (Member s xs) || Get s xs == Just Integer) ~ True)
        => Proxy s -> Integer -> P xs xs (Either Reply Integer)
incrby key n = P $ Redis.incrby (encodeKey key) n

incrbyfloat :: (KnownSymbol s, (Not (Member s xs) || Get s xs == Just Double) ~ True)
        => Proxy s -> Double -> P xs xs (Either Reply Double)
incrbyfloat key n = P $ Redis.incrbyfloat (encodeKey key) n

psetex :: (KnownSymbol s, Value x)
        => Proxy s -> Integer -> x -> P xs (Set s x xs) (Either Reply Status)
psetex key n val = P $ Redis.psetex (encodeKey key) n (enc val)

set :: (Value x, KnownSymbol s)
        => Proxy s -> x -> P xs (Set s x xs) (Either Reply Status)
set key val = P $ Redis.set (encodeKey key) (enc val)

setbit :: (Value x, KnownSymbol s)
        => Proxy s -> Integer -> x -> P xs (Set s x xs) (Either Reply Integer)
setbit key n val = P $ Redis.setbit (encodeKey key) n (enc val)

setex :: (Value x, KnownSymbol s)
        => Proxy s -> Integer -> x -> P xs (Set s x xs) (Either Reply Status)
setex key n val = P $ Redis.setex (encodeKey key) n (enc val)

setrange :: (Value x, KnownSymbol s)
        => Proxy s -> Integer -> x -> P xs (Set s x xs) (Either Reply Integer)
setrange key n val = P $ Redis.setrange (encodeKey key) n (enc val)

strlen :: ByteString -> P xs xs (Either Reply Integer)
strlen key = P $ Redis.strlen key




--------------------------------------------------------------------------------
--  List
--------------------------------------------------------------------------------

-- LPUSH s x xs = Member s xs ~ False || Get s xs ~ Just (ListK x)
type family LPUSH (s :: Symbol) (x :: *) (xs :: [ (Symbol, *) ]) :: Bool where
    LPUSH s x '[]                   = True
    LPUSH s x ('(s, ListK x) ': xs) = True
    LPUSH s x ('(s, y)       ': xs) = False
    LPUSH s x ('(t, y)       ': xs) = LPUSH s x xs

lpush :: (LPUSH s x xs ~ True, Value x, KnownSymbol s)
      => Proxy s -> x -> P xs (Set s (ListK x) xs) (Either Reply Integer)
lpush key val = P $ Redis.lpush (enc $ symbolVal key) [enc val]

lpop :: (FromJust (Get s xs) ~ ListK x, Value x, KnownSymbol s)
     => Proxy s -> P xs xs (Either Reply (Maybe x))
lpop key = P $ Redis.lpop (enc $ symbolVal key) >>= decodeAsMaybe

llen :: (FromJust (Get s xs) ~ ListK x, KnownSymbol s)
     => Proxy s -> P xs xs (Either Reply Integer)
llen key = P $ Redis.llen (enc $ symbolVal key)

lindex :: (FromJust (Get s xs) ~ ListK x, Value x, KnownSymbol s)
     => Proxy s -> Integer -> P xs xs (Either Reply (Maybe x))
lindex key index = P $ Redis.lindex (enc $ symbolVal key) index >>= decodeAsMaybe

lrange :: (FromJust (Get s xs) ~ ListK x, Value x, KnownSymbol s)
     => Proxy s -> Integer -> Integer -> P xs xs (Either Reply [x])
lrange key from to = P $ Redis.lrange (enc $ symbolVal key) from to >>= decodeAsList

--------------------------------------------------------------------------------
--  Set
--------------------------------------------------------------------------------

-- SADD s x xs = Member s xs ~ False || Get s xs ~ Just (SetK x)
type family SADD (s :: Symbol) (x :: *) (xs :: [ (Symbol, *) ]) :: Bool where
    SADD s x '[]                  = True
    SADD s x ('(s, SetK x) ': xs) = True
    SADD s x ('(s, y)      ': xs) = False
    SADD s x ('(t, y)      ': xs) = SADD s x xs

sadd :: (SADD s x xs ~ True, Value x, KnownSymbol s)
     => Proxy s -> x -> P xs (Set s (SetK x) xs)  (Either Reply Integer)
sadd key val = P $ Redis.sadd (enc $ symbolVal key) [enc val]

srem :: (Member s xs ~ True, FromJust (Get s xs) ~ SetK x, Value x, KnownSymbol s)
     => Proxy s -> x -> P xs xs (Either Reply Integer)
srem key val = P $ Redis.srem (enc $ symbolVal key) [enc val]

scard :: (Member s xs ~ True, FromJust (Get s xs) ~ SetK x, Value x, KnownSymbol s)
      => Proxy s -> P xs xs  (Either Reply Integer)
scard key = P $ Redis.scard (enc $ symbolVal key)

smembers :: (Member s xs ~ True, FromJust (Get s xs) ~ SetK x, Value x, KnownSymbol s)
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

decodeAsList :: (Value x) => Either Reply [ByteString] -> Redis (Either Reply [x])
decodeAsList (Left replyErr) = return $ Left replyErr
decodeAsList (Right strs) = case mapM dec strs of
    Left decodeErr -> return $ Left (Error $ enc decodeErr)
    Right vals -> return $ Right vals

fromRight :: Either a b -> b
fromRight (Left e) = error "Left val"
fromRight (Right e) = e

fromMaybeResult :: Either Reply (Maybe x) -> x
fromMaybeResult = fromJust . fromRight
