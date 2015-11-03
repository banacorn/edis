{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies, TypeOperators
    , OverloadedStrings, MultiParamTypeClasses #-}

module Edis.Promoted where

import Edis.Type
import Edis.Dict
import Edis.Serialize
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)
import Data.Maybe (fromJust)

import Database.Redis as Redis
import           Control.Monad.State (liftIO)
import Data.Proxy
import GHC.TypeLits


start :: P '[] '[] ()
start = P (return ())

--------------------------------------------------------------------------------
--  Connection
--------------------------------------------------------------------------------

ping :: P xs xs (Either Reply Redis.Status)
ping = P Redis.ping

--------------------------------------------------------------------------------
--  Declaration
--------------------------------------------------------------------------------

dec :: (Member s xs ~ False, KnownSymbol s)
    => Proxy s -> Proxy x -> P xs (Set s x xs) ()
dec s x = P $ return ()

--------------------------------------------------------------------------------
--  String
--------------------------------------------------------------------------------

set :: (Value x, KnownSymbol s)
    => Proxy s -> x -> P xs (Set s x xs) (Either Reply Redis.Status)
set key val = P $ Redis.set (en $ symbolVal key) (en val)

get :: (Value x, x ~ FromJust (Get s xs), Member s xs ~ True, KnownSymbol s)
    => Proxy s -> P xs xs (Either Reply (Maybe x))
get key = P $ Redis.get (en $ symbolVal key) >>= decodeAsMaybe

del :: (Member s xs ~ True, KnownSymbol s)
    => Proxy s -> P xs (Del s xs) (Either Reply Integer)
del key = P $ Redis.del [en $ symbolVal key]

incr :: (FromJust (Get s xs) ~ Integer, Member s xs ~ True, KnownSymbol s)
     => Proxy s -> P xs xs (Either Reply Integer)
incr key = P $ Redis.incr (en $ symbolVal key)

decr :: (FromJust (Get s xs) ~ Integer, Member s xs ~ True, KnownSymbol s)
     => Proxy s -> P xs xs (Either Reply Integer)
decr key = P $ Redis.decr (en $ symbolVal key)

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
lpush key val = P $ Redis.lpush (en $ symbolVal key) [en val]

lpop :: (FromJust (Get s xs) ~ ListK x, Value x, KnownSymbol s)
     => Proxy s -> P xs xs (Either Reply (Maybe x))
lpop key = P $ Redis.lpop (en $ symbolVal key) >>= decodeAsMaybe

llen :: (FromJust (Get s xs) ~ ListK x, KnownSymbol s)
     => Proxy s -> P xs xs (Either Reply Integer)
llen key = P $ Redis.llen (en $ symbolVal key)

lindex :: (FromJust (Get s xs) ~ ListK x, Value x, KnownSymbol s)
     => Proxy s -> Integer -> P xs xs (Either Reply (Maybe x))
lindex key index = P $ Redis.lindex (en $ symbolVal key) index >>= decodeAsMaybe

lrange :: (FromJust (Get s xs) ~ ListK x, Value x, KnownSymbol s)
     => Proxy s -> Integer -> Integer -> P xs xs (Either Reply [x])
lrange key from to = P $ Redis.lrange (en $ symbolVal key) from to >>= decodeAsList

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
sadd key val = P $ Redis.sadd (en $ symbolVal key) [en val]

srem :: (Member s xs ~ True, FromJust (Get s xs) ~ SetK x, Value x, KnownSymbol s)
     => Proxy s -> x -> P xs xs (Either Reply Integer)
srem key val = P $ Redis.srem (en $ symbolVal key) [en val]

scard :: (Member s xs ~ True, FromJust (Get s xs) ~ SetK x, Value x, KnownSymbol s)
      => Proxy s -> P xs xs  (Either Reply Integer)
scard key = P $ Redis.scard (en $ symbolVal key)

smembers :: (Member s xs ~ True, FromJust (Get s xs) ~ SetK x, Value x, KnownSymbol s)
         => Proxy s -> P xs xs  (Either Reply [x])
smembers key = P $ Redis.smembers (en $ symbolVal key) >>= decodeAsList

--------------------------------------------------------------------------------
--  Helper functions
--------------------------------------------------------------------------------

decodeAsMaybe :: (Value x) => Either Reply (Maybe ByteString) -> Redis (Either Reply (Maybe x))
decodeAsMaybe (Left replyErr) = return $ Left replyErr
decodeAsMaybe (Right Nothing) = return $ Right Nothing
decodeAsMaybe (Right (Just str)) = case de str of
        Left decodeErr -> return $ Left (Error $ pack decodeErr)
        Right val -> return $ Right (Just val)

decodeAsList :: (Value x) => Either Reply [ByteString] -> Redis (Either Reply [x])
decodeAsList (Left replyErr) = return $ Left replyErr
decodeAsList (Right strs) = case mapM de strs of
    Left decodeErr -> return $ Left (Error $ pack decodeErr)
    Right vals -> return $ Right vals

fromRight :: Either a b -> b
fromRight (Left e) = error "Left val"
fromRight (Right e) = e

fromMaybeResult :: Either Reply (Maybe x) -> x
fromMaybeResult = fromJust . fromRight
