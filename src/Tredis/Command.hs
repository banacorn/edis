{-# LANGUAGE GADTs, OverloadedStrings, RankNTypes, TypeOperators #-}

module Tredis.Command where

import Tredis.Transaction
import Tredis.Serialize
import Tredis.TypeChecking
import Data.Typeable
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 (pack, unpack)
import           Data.Serialize (Serialize)
import           Database.Redis (sendRequest, Status(..))

--------------------------------------------------------------------------------
--  Connection
--------------------------------------------------------------------------------

ping :: Tx Status
ping = returnStatus ["PING"]

--------------------------------------------------------------------------------
--  String
--------------------------------------------------------------------------------

declare :: (Se a, Typeable a) => Key -> Tx a
declare key = do
    let Right val = de "witchcraft" -- fake a value
    insertType key (typeOf val)
    return $ Deferred $ \_ -> Right val

set :: (Se a, Typeable a) => Key -> a -> Tx Status
set key val = checkType' key (returnStatus ["SET", key, en val]) (const $ typeOf val)

incr :: Key -> Tx Int
incr key = checkType' key (returnInt ["INCR", key]) (const $ typeRep (Proxy :: Proxy Int))

decr :: Key -> Tx Int
decr key = checkType' key (returnInt ["DECR", key]) (const $ typeRep (Proxy :: Proxy Int))

get :: (Se a, Typeable a) => Key -> Tx (Maybe a)
get key = checkType' key (returnMaybe ["GET", key]) (carrier . deferred)

del :: Key -> Tx Status
del key = do
    removeType key
    returnStatus ["DEL", key]

--------------------------------------------------------------------------------
--  List
--------------------------------------------------------------------------------

lpush :: (Se a, Typeable a) => Key -> a -> Tx Status
lpush key val = checkType' key (returnStatus ["LPUSH", key, en val]) (const $ typeOf $ List val)

lpop :: (Se a, Typeable a) => Key -> Tx (Maybe a)
lpop key = checkType' key (returnMaybe ["LPOP", key]) (list . carrier . deferred)

-- llen :: Key -> Tx Int
-- llen key = do
--     returnInt ["LLEN", key]

lrange :: (Se a, Typeable a) => Key -> Integer -> Integer -> Tx [a]
lrange key m n = checkType' key (returnList ["LRANGE", key, en m, en n]) (list . carrier . deferred)

lindex :: (Se a, Typeable a) => Key -> Integer -> Tx (Maybe a)
lindex key n = checkType' key (returnMaybe ["LINDEX", key, en n]) (list . carrier . deferred)

--------------------------------------------------------------------------------
--  Set
--------------------------------------------------------------------------------

-- sadd :: (Se a, Typeable a) => Key -> a -> Tx Status
-- sadd key val = do
--     typeError <- checkType key (typeOf $ Set val)
--     case typeError of
--         Just err -> do
--             assertError err
--         Nothing -> returnStatus ["SADD", key, en val]
--
-- -- scard :: Set a -> Int
-- scard :: Key -> Tx Int
-- scard key = do
--     -- checkType key (list $ carrier $ deferred val)
--     returnInt ["SCARD", key]
