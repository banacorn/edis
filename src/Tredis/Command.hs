{-# LANGUAGE GADTs, OverloadedStrings, RankNTypes, TypeOperators #-}

module Tredis.Command where

import Tredis.Transaction
import Tredis.Serialize
import Data.Typeable
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 (pack, unpack)
import           Data.Serialize (Serialize)
import           Database.Redis (sendRequest, Status(..))

--------------------------------------------------------------------------------
--  String
--------------------------------------------------------------------------------

declare :: (Se a, Typeable a) => Key -> Tx a
declare key = do
    let Right val = de "witchcraft" -- fake a value
    key =:: val                         -- see if their type matches
    return val

set :: (Se a, Typeable a) => Key -> a -> Tx (Deferred ())
set key val = do
    key =:: val
    -- sendCommand' decodeAsStatus ["SET", key, en val]
    sendCommand ["SET", key, en val]

incr :: Key -> Tx (Deferred ())
incr key = do
    typeError <- checkType key (typeRep (Proxy :: Proxy Int))
    case typeError of
        Just err -> do
            assertError err
            return $ error (show err)
        Nothing -> sendCommand ["INCR", key]

decr :: Key -> Tx (Deferred ())
decr key = do
    typeError <- checkType key (typeRep (Proxy :: Proxy Int))
    case typeError of
        Just err -> do
            assertError err
            return $ error (show err)
        Nothing -> sendCommand ["DECR", key]

get :: (Se a, Typeable a) => Key -> Tx (Deferred (Maybe a))
get key = do
    val <- sendCommand' decodeAsMaybe ["GET", key]
    let deferredType = deferredValueType val
    typeError <- checkType key (head $ typeRepArgs deferredType)
    case typeError of
        Just er -> do
            assertError er
            return $ error (show er)
        Nothing -> return val

del :: Key -> Tx (Deferred ())
del key = do
    removeType key
    sendCommand ["DEL", key]

--------------------------------------------------------------------------------
--  List
--------------------------------------------------------------------------------

lpush :: (Se a, Typeable a) => Key -> a -> Tx (Deferred ())
lpush key val = do
    key =:: [val]
    sendCommand ["LPUSH", key, en val]

lpop :: (Se a, Typeable a) => Key -> Tx (Deferred (Maybe a))
lpop key = do
    val <- sendCommand' decodeAsMaybe ["LPOP", key]
    let deferredType = deferredValueType val
    let listType = buildListType (head $ typeRepArgs deferredType)
    typeError <- checkType key listType
    case typeError of
        Just er -> do
            assertError er
            return $ error (show er)
        Nothing -> return val

llen :: Key -> Tx (Deferred Int)
llen key = do
    sendCommand' decodeAsInt ["LLEN", key]

lrange :: (Se a, Typeable a) => Key -> Integer -> Integer -> Tx (Deferred [a])
lrange key m n = do
    val <- sendCommand' decodeAsList ["LRANGE", key, en m, en n]
    let deferredType = deferredValueType val
    typeError <- checkType key deferredType
    case typeError of
        Just er -> do
            assertError er
            return $ error (show er)
        Nothing -> return val

lindex :: (Se a, Typeable a) => Key -> Integer -> Tx (Deferred (Maybe a))
lindex key n = do
    val <- sendCommand' decodeAsMaybe ["LINDEX", key, en n]
    let deferredType = deferredValueType val
    let listType = buildListType (head $ typeRepArgs deferredType)
    typeError <- checkType key listType
    case typeError of
        Just er -> do
            assertError er
            return $ error (show er)
        Nothing -> return val

--------------------------------------------------------------------------------
--  Set
--------------------------------------------------------------------------------

sadd :: (Se a, Typeable a) => Key -> a -> Tx (Deferred ())
sadd key val = do
    -- key =:: Set val
    sendCommand ["SADD", key, en val]

scard :: Key -> Tx (Deferred Int)
scard key  = do
    sendCommand' decodeAsInt ["SCARD", key]
