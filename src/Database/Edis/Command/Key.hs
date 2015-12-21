{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, OverloadedStrings #-}

module Database.Edis.Command.Key where

import Database.Edis.Type
import Database.Edis.Helper

import Data.ByteString          (ByteString)
import Data.Proxy               (Proxy)
import Data.Serialize           (Serialize)
import Data.Type.Bool
import Data.Type.Equality
import Database.Redis as Redis hiding (decode)
import GHC.TypeLits


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
rename :: (KnownSymbol s, KnownSymbol t, Member xs s ~ 'True, Get xs s ~ 'Just x, (s == t) ~ 'False)
        => Proxy s -> Proxy t -> Edis xs (Set (Del xs s) t x) (Either Reply Status)
rename key key' = Edis $ Redis.rename (encodeKey key) (encodeKey key')



-- 1. the old key must exist
-- 2. the old key and the new key must be different
-- 3. if the new key exists
--      then nothing happens
--      else the new key is removed
renamenx :: (KnownSymbol s, KnownSymbol t, Member xs s ~ 'True, Get xs s ~ 'Just x, (s == t) ~ 'False)
        => Proxy s -> Proxy t -> Edis xs (If (Member xs t) xs (Del xs s)) (Either Reply Bool)
renamenx key key' = Edis $ Redis.renamenx (encodeKey key) (encodeKey key')

restore :: ByteString -> Integer -> ByteString -> Edis xs xs (Either Reply Status)
restore key n val = Edis $ Redis.restore key n val

-- must be a List, a Set or a Sorted Set
sort :: (KnownSymbol s, Serialize x, Member xs s ~ 'True
        , (Get xs s == 'Just (ListOf x) || Get xs s == 'Just (SetOf x) || Get xs s == 'Just (ZSetOf x)) ~ 'True)
        => Proxy s -> SortOpts -> Edis xs xs (Either Reply [x])
sort key opt = Edis $ Redis.sort (encodeKey key) opt >>= decodeAsList

sortStore :: (KnownSymbol s, KnownSymbol t, Member xs s ~ 'True, FromJust (Get xs s) ~ x
        , (IsList (FromJust (Get xs s)) || IsSet (FromJust (Get xs s)) || IsZSet (FromJust (Get xs s))) ~ 'True)
        => Proxy s -> Proxy t -> SortOpts -> Edis xs (Set xs s x) (Either Reply Integer)
sortStore key dest opt = Edis $ Redis.sortStore (encodeKey key) (encodeKey dest) opt

ttl :: ByteString -> Edis xs xs (Either Reply Integer)
ttl key = Edis $ Redis.ttl key

getType :: ByteString -> Edis xs xs (Either Reply RedisType)
getType key = Edis $ Redis.getType key
