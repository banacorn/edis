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
ping = sendCommand decodeAsStatus ["PING"]

--------------------------------------------------------------------------------
--  String
--------------------------------------------------------------------------------

declare :: (Se a, Typeable a) => Key -> Tx a
declare key = do
    let Right val = de "witchcraft" -- fake a value
    insertType key (typeOf val)
    return $ Deferred $ \_ -> Right val

set :: (Se a, Typeable a) => Key -> a -> Tx Status
set key val = do
    typeError <- checkType key (typeOf val)
    case typeError of
        Just err -> do
            assertError err
            error (show err)
        Nothing -> sendCommand decodeAsStatus ["SET", key, en val]

incr :: Key -> Tx Int
incr key = do
    typeError <- checkType key (typeRep (Proxy :: Proxy Int))
    case typeError of
        Just err -> do
            assertError err
            error (show err)
        Nothing -> sendCommand decodeAsInt ["INCR", key]

decr :: Key -> Tx Int
decr key = do
    typeError <- checkType key (typeRep (Proxy :: Proxy Int))
    case typeError of
        Just err -> do
            assertError err
            error (show err)
        Nothing -> sendCommand decodeAsInt ["DECR", key]

get :: (Se a, Typeable a) => Key -> Tx (Maybe a)
get key = do
    val <- sendCommand decodeAsMaybe ["GET", key]
    typeError <- checkType key (carrier $ deferred val)
    case typeError of
        Just er -> do
            assertError er
            return $ error (show er)
        Nothing -> return val

del :: Key -> Tx Status
del key = do
    removeType key
    sendCommand decodeAsStatus ["DEL", key]

--------------------------------------------------------------------------------
--  List
--------------------------------------------------------------------------------

lpush :: (Se a, Typeable a) => Key -> a -> Tx Status
lpush key val = do
    typeError <- checkType key (typeOf $ List val)
    case typeError of
        Just err -> do
            assertError err
            error (show err)
        Nothing -> sendCommand decodeAsStatus ["LPUSH", key, en val]


lpop :: (Se a, Typeable a) => Key -> Tx (Maybe a)
lpop key = do
    val <- sendCommand decodeAsMaybe ["LPOP", key]
    typeError <- checkType key (list $ carrier $ deferred val)
    case typeError of
        Just er -> do
            assertError er
            return $ error (show er)
        Nothing -> return val

llen :: Key -> Tx Int
llen key = do
    sendCommand decodeAsInt ["LLEN", key]

lrange :: (Se a, Typeable a) => Key -> Integer -> Integer -> Tx [a]
lrange key m n = do
    val <- sendCommand decodeAsList ["LRANGE", key, en m, en n]
    typeError <- checkType key (deferred val)
    case typeError of
        Just er -> do
            assertError er
            return $ error (show er)
        Nothing -> return val

lindex :: (Se a, Typeable a) => Key -> Integer -> Tx (Maybe a)
lindex key n = do
    val <- sendCommand decodeAsMaybe ["LINDEX", key, en n]
    typeError <- checkType key (list $ carrier $ deferred val)
    case typeError of
        Just er -> do
            assertError er
            return $ error (show er)
        Nothing -> return val

--------------------------------------------------------------------------------
--  Set
--------------------------------------------------------------------------------

sadd :: (Se a, Typeable a) => Key -> a -> Tx Status
sadd key val = do
    typeError <- checkType key (typeOf $ Set val)
    case typeError of
        Just err -> do
            assertError err
            error (show err)
        Nothing -> sendCommand decodeAsStatus ["SADD", key, en val]

-- scard :: Set a -> Int
scard :: Key -> Tx Int
scard key = do
    -- checkType key (list $ carrier $ deferred val)
    sendCommand decodeAsInt ["SCARD", key]
