{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, OverloadedStrings #-}

module Database.Edis.Command.List where

import Database.Edis.Type
import Database.Edis.Helper

import Data.ByteString          (ByteString)
import Data.Proxy               (Proxy)
import Data.Serialize           (Serialize, encode)
import Data.Type.Bool
import Database.Redis as Redis hiding (decode)
import GHC.TypeLits


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

lpop :: (KnownSymbol s, Serialize x, 'Just (ListOf x) ~ Get xs s)
        => Proxy s
        -> Edis xs xs (Either Reply (Maybe x))
lpop key = Edis $ Redis.lpop (encodeKey key) >>= decodeAsMaybe

lpush :: (KnownSymbol s, Serialize x, ListOrNX xs s)
      => Proxy s -> x
      -> Edis xs (Set xs s (ListOf x)) (Either Reply Integer)
lpush key val = Edis $ Redis.lpush (encodeKey key) [encode val]

-- no operation will be performed when key does not yet exist.
lpushx :: (KnownSymbol s, Serialize x, 'Just (ListOf x) ~ Get xs s)
        => Proxy s -> x
        -> Edis xs xs (Either Reply Integer)
lpushx key val = Edis $ Redis.lpushx (encodeKey key) (encode val)

lrange :: (KnownSymbol s, Serialize x, ListOrNX xs s)
     => Proxy s -> Integer -> Integer
     -> Edis xs xs (Either Reply [x])
lrange key from to = Edis $ Redis.lrange (encodeKey key) from to >>= decodeAsList

lrem :: (KnownSymbol s, Serialize x, ListOrNX xs s)
      => Proxy s -> Integer -> x
      -> Edis xs xs (Either Reply Integer)
lrem key count val = Edis $ Redis.lrem (encodeKey key) count (encode val)

lset :: (KnownSymbol s, Serialize x, IsList (FromJust (Get xs s)) ~ 'True)
      => Proxy s -> Integer -> x
      -> Edis xs xs (Either Reply Status)
lset key index val = Edis $ Redis.lset (encodeKey key) index (encode val)

ltrim :: (KnownSymbol s, ListOrNX xs s)
      =>  Proxy s -> Integer -> Integer
      -> Edis xs xs (Either Reply Status)
ltrim key from to = Edis $ Redis.ltrim (encodeKey key) from to

rpop :: (KnownSymbol s, Serialize x, 'Just (ListOf x) ~ Get xs s)
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
rpushx :: (KnownSymbol s, Serialize x, 'Just (ListOf x) ~ Get xs s)
        => Proxy s -> x
        -> Edis xs xs (Either Reply Integer)
rpushx key val = Edis $ Redis.rpushx (encodeKey key) (encode val)
