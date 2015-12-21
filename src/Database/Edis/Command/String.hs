{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, OverloadedStrings #-}

module Database.Edis.Command.String where

import Database.Edis.Type
import Database.Edis.Helper

import Data.ByteString          (ByteString)
import Data.Proxy               (Proxy)
import Data.Serialize           (Serialize, encode)
import Data.Type.Bool
import Database.Redis as Redis hiding (decode)
import GHC.TypeLits


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
