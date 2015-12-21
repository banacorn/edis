{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies, TypeOperators, OverloadedStrings #-}

module Database.Edis.Command.Hash where

import Database.Edis.Type
import Database.Edis.Helper

import Data.ByteString          (ByteString)
import Data.Proxy               (Proxy)
import Data.Serialize           (Serialize, encode)
import Data.Type.Bool
import Database.Redis as Redis hiding (decode)
import GHC.TypeLits

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
        , 'Just (StringOf x) ~ GetHash xs k f)
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
