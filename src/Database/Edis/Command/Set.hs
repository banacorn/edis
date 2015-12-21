{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies, TypeOperators, OverloadedStrings #-}

module Database.Edis.Command.Set where

import Database.Edis.Type
import Database.Edis.Helper

import Data.Proxy               (Proxy)
import Data.Serialize           (Serialize, encode)
import Data.Type.Bool
import Database.Redis as Redis hiding (decode)
import GHC.TypeLits

--------------------------------------------------------------------------------
--  Sets
--------------------------------------------------------------------------------

sadd :: (KnownSymbol s, Serialize x, SetOrNX xs s)
        => Proxy s -> x
        -> Edis xs (If (Member xs s) xs (Set xs s (SetOf x))) (Either Reply Integer)
sadd key val = Edis $ Redis.sadd (encodeKey key) [encode val]

scard :: (KnownSymbol s, SetOrNX xs s)
        => Proxy s
        -> Edis xs xs (Either Reply Integer)
scard key = Edis $ Redis.scard (encodeKey key)

sdiff :: (KnownSymbol s, KnownSymbol t, Serialize x
        , 'Just (SetOf x) ~ Get xs s  -- must exist
        , SetOrNX xs s)
        => Proxy s -> Proxy t
        -> Edis xs xs (Either Reply [x])
sdiff key key' = Edis $ Redis.sdiff [encodeKey key, encodeKey key'] >>= decodeAsList

sdiffstore :: (KnownSymbol s, KnownSymbol t, SetOrNX xs s, SetOrNX xs t)
        => Proxy s -> Proxy t
        -> Edis xs xs (Either Reply Integer)
sdiffstore dest key = Edis $ Redis.sdiffstore (encodeKey dest) [encodeKey key]

sinter :: (KnownSymbol s, KnownSymbol t, Serialize x
        , 'Just (SetOf x) ~ Get xs s  -- must exist
        , SetOrNX xs s)
        => Proxy s -> Proxy t
        -> Edis xs xs (Either Reply [x])
sinter key key' = Edis $ Redis.sinter [encodeKey key, encodeKey key'] >>= decodeAsList

sinterstore :: (KnownSymbol s, KnownSymbol t, SetOrNX xs s, SetOrNX xs t)
        => Proxy s -> Proxy t
        -> Edis xs xs (Either Reply Integer)
sinterstore dest key = Edis $ Redis.sinterstore (encodeKey dest) [encodeKey key]

smembers :: (KnownSymbol s, Serialize x
          , 'Just (SetOf x) ~ Get xs s)
         => Proxy s
         -> Edis xs xs  (Either Reply [x])
smembers key = Edis $ Redis.smembers (encodeKey key) >>= decodeAsList

--  if the `dest` is not a set then the `src` must not exist
--  else the `src` and `dest` may be a set or does not exist.
smove :: (KnownSymbol s, KnownSymbol t, Serialize x
        , (Not (Member xs s) && Not (IsSet (FromJust (Get xs t)))
            || SetOrNX' xs s && SetOrNX' xs t) ~ 'True)
      => Proxy s -> Proxy t -> x -> Edis xs (Set xs t (SetOf x)) (Either Reply Bool)
smove src dest val = Edis $ Redis.smove (encodeKey src) (encodeKey dest) (encode val)

spop :: (KnownSymbol s, Serialize x
      , 'Just (SetOf x) ~ Get xs s)
     => Proxy s
     -> Edis xs xs (Either Reply (Maybe x))
spop key = Edis $ Redis.spop (encodeKey key) >>= decodeAsMaybe

srandmember :: (KnownSymbol s, Serialize x
             , 'Just (SetOf x) ~ Get xs s)
            => Proxy s
            -> Edis xs xs (Either Reply (Maybe x))
srandmember key = Edis $ Redis.srandmember (encodeKey key) >>= decodeAsMaybe

srem :: (KnownSymbol s, Serialize x
      , 'Just (SetOf x) ~ Get xs s)
     => Proxy s
     -> x
     -> Edis xs xs (Either Reply Integer)
srem key val = Edis $ Redis.srem (encodeKey key) [encode val]

sunion :: (KnownSymbol s, KnownSymbol t, Serialize x
        , 'Just (SetOf x) ~ Get xs s  -- must exist
        , SetOrNX xs s)
        => Proxy s -> Proxy t
        -> Edis xs xs (Either Reply [x])
sunion key key' = Edis $ Redis.sunion [encodeKey key, encodeKey key'] >>= decodeAsList

sunionstore :: (KnownSymbol s, KnownSymbol t, SetOrNX xs s, SetOrNX xs t)
        => Proxy s -> Proxy t
        -> Edis xs xs (Either Reply Integer)
sunionstore dest key = Edis $ Redis.sunionstore (encodeKey dest) [encodeKey key]
