{-# LANGUAGE OverloadedStrings #-}

module Tredis.Command where

import Tredis.Transaction
import Tredis.Serialize
import Tredis.TypeChecking
import Tredis.Type
import Data.Typeable

import Database.Redis (Status(..))

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

    let (tyCon, tyArgs) = splitTyConApp (typeOf val)
    if tyCon == typeRepTyCon list
        then    insertType key (List' $ head tyArgs)
        else    insertType key (Type $ typeOf val)
    return $ Deferred $ \_ -> Right val

set :: (Se a, Typeable a) => Key -> a -> Tx Status
set key val = compareType key (returnStatus ["SET", key, en val]) (Type (typeOf val))

incr :: Key -> Tx Int
incr key = compareType key (returnInt ["INCR", key]) (Type int)

decr :: Key -> Tx Int
decr key = compareType key (returnInt ["DECR", key]) (Type int)

get :: (Se a, Typeable a) => Key -> Tx (Maybe a)
get key = compareCmdType key (returnMaybe ["GET", key]) (Type . carrier . deferred)

del :: Key -> Tx Status
del key = do
    removeType key
    returnStatus ["DEL", key]

--------------------------------------------------------------------------------
--  List
--------------------------------------------------------------------------------

lpush :: (Se a, Typeable a) => Key -> a -> Tx Int
lpush key val = compareType key (returnInt ["LPUSH", key, en val]) (List' (typeOf val))

lpop :: (Se a, Typeable a) => Key -> Tx (Maybe a)
lpop key = compareCmdType key (returnMaybe ["LPOP", key]) (List' . carrier . deferred)

llen :: Key -> Tx Int
llen key = compareType key (returnInt ["LLEN", key]) ListOfAnything

lrange :: (Se a, Typeable a) => Key -> Integer -> Integer -> Tx [a]
lrange key m n = compareCmdType key (returnList  ["LRANGE", key, en m, en n]) (List' . carrier . deferred)

lindex :: (Se a, Typeable a) => Key -> Integer -> Tx (Maybe a)
lindex key n = compareCmdType key (returnMaybe ["LINDEX", key, en n]) (List' . carrier . deferred)

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
