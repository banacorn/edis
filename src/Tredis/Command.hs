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
    key =:: val                     -- see if their type matches
    return $ Deferred $ \_ -> Right val

set :: (Se a, Typeable a) => Key -> a -> Tx ()
set key val = do
    key =:: val
    sendCommand ["SET", key, en val]

incr :: Key -> Tx ()
incr key = do
    typeError <- checkType key (typeRep (Proxy :: Proxy Int))
    case typeError of
        Just err -> do
            assertError err
            return $ error (show err)
        Nothing -> sendCommand ["INCR", key]

decr :: Key -> Tx ()
decr key = do
    typeError <- checkType key (typeRep (Proxy :: Proxy Int))
    case typeError of
        Just err -> do
            assertError err
            return $ error (show err)
        Nothing -> sendCommand ["DECR", key]

get :: (Se a, Typeable a) => Key -> Tx (Maybe a)
get key = do
    val <- sendCommand' decodeAsMaybe ["GET", key]
    typeError <- checkType key (carrier $ deferred val)
    case typeError of
        Just er -> do
            assertError er
            return $ error (show er)
        Nothing -> return val

del :: Key -> Tx ()
del key = do
    removeType key
    sendCommand ["DEL", key]

--------------------------------------------------------------------------------
--  List
--------------------------------------------------------------------------------

lpush :: (Se a, Typeable a) => Key -> a -> Tx ()
lpush key val = do
    key =:: List val
    sendCommand ["LPUSH", key, en val]

lpop :: (Se a, Typeable a) => Key -> Tx (Maybe a)
lpop key = do
    val <- sendCommand' decodeAsMaybe ["LPOP", key]
    typeError <- checkType key (list $ carrier $ deferred val)
    case typeError of
        Just er -> do
            assertError er
            return $ error (show er)
        Nothing -> return val

llen :: Key -> Tx Int
llen key = do
    sendCommand' decodeAsInt ["LLEN", key]

lrange :: (Se a, Typeable a) => Key -> Integer -> Integer -> Tx [a]
lrange key m n = do
    val <- sendCommand' decodeAsList ["LRANGE", key, en m, en n]
    typeError <- checkType key (deferred val)
    case typeError of
        Just er -> do
            assertError er
            return $ error (show er)
        Nothing -> return val

lindex :: (Se a, Typeable a) => Key -> Integer -> Tx (Maybe a)
lindex key n = do
    val <- sendCommand' decodeAsMaybe ["LINDEX", key, en n]
    typeError <- checkType key (list $ carrier $ deferred val)
    case typeError of
        Just er -> do
            assertError er
            return $ error (show er)
        Nothing -> return val

--------------------------------------------------------------------------------
--  Set
--------------------------------------------------------------------------------

sadd :: (Se a, Typeable a) => Key -> a -> Tx ()
sadd key val = do
    -- key =:: Set val
    sendCommand ["SADD", key, en val]

scard :: Key -> Tx Int
scard key  = do
    sendCommand' decodeAsInt ["SCARD", key]
