{-# LANGUAGE GADTs, OverloadedStrings, RankNTypes, TypeOperators #-}

module Tredis.Command where

import Tredis.Transaction
import Data.Typeable
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 (pack, unpack)
import           Data.Serialize (Serialize, encode, decode)
import           Database.Redis (sendRequest)

--------------------------------------------------------------------------------
--  String
--------------------------------------------------------------------------------

declare :: (Serialize a, Typeable a) => Key -> Tx a
declare key = do
    let Right val = decode "witchcraft" -- fake a value
    key =:: val                         -- see if their type matches
    return val

set :: (Serialize a, Typeable a) => Key -> a -> Tx (Queued ())
set key val = do
    key =:: val
    insertCmd ["SET", key, encode val]

get :: (Serialize a, Typeable a) => Key -> Tx (Queued a)
get key = do
    val <- insertCmd ["GET", key]
    let queuedType = queuedValueType val
    typeError <- checkType key queuedType
    case typeError of
        Just er -> do
            assertError er
            return $ error (show er)
        Nothing -> return val

del :: Key -> Tx (Queued ())
del key = do
    removeType key
    insertCmd ["DEL", key]

--------------------------------------------------------------------------------
--  List
--------------------------------------------------------------------------------

lpush :: (Serialize a, Typeable a) => Key -> a -> Tx (Queued ())
lpush key val = do
    key =:: [val]
    insertCmd ["LPUSH", key, encode val]

lpop :: (Serialize a, Typeable a) => Key -> Tx (Queued a)
lpop key = do
    val <- insertCmd ["LPOP", key]
    let queuedType = queuedValueType val
    let listType = buildListType queuedType
    typeError <- checkType key listType
    case typeError of
        Just er -> do
            assertError er
            return $ error (show er)
        Nothing -> return val
