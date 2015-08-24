{-# LANGUAGE OverloadedStrings #-}

module Tredis.Command where

import Tredis.Transaction
import Tredis.Serialize
import Tredis.TypeChecking
import Tredis.Type
import Data.Typeable

--------------------------------------------------------------------------------
--  Connection
--------------------------------------------------------------------------------

ping :: Tx Status
ping = returnStatus PING

--------------------------------------------------------------------------------
--  String
--------------------------------------------------------------------------------

declare :: Value a => Key -> Tx a
declare key = do
    let Right val = de "witchcraft" -- fake a value
    insertType key (typeToInsert val)
    return $ Deferred $ \_ -> Right val
    where   typeToInsert val
                | tyCon == listTyCon = ListType  $ head tyArgs
                | tyCon == setTyCon = SetType  $ head tyArgs
                | otherwise = Type $ typeOf val
                where   (tyCon, tyArgs) = splitTyConApp (typeOf val)
                        listTyCon = typeRepTyCon listTypeRep
                        setTyCon = typeRepTyCon setTypeRep

set :: Value a => Key -> a -> Tx Status
set key val = compareType key (returnStatus (SET key val)) (Type (typeOf val))

incr :: Key -> Tx Int
incr key = compareType key (returnInt (INCR key)) (Type intTypeRep)

decr :: Key -> Tx Int
decr key = compareType key (returnInt (DECR key)) (Type intTypeRep)

get :: Value a => Key -> Tx (Maybe a)
get key = compareCmdType key (returnMaybe (GET key)) (Type . carrier . deferred)

del :: Key -> Tx Status
del key = do
    removeType key
    returnStatus (DEL key)
--
-- --------------------------------------------------------------------------------
-- --  List
-- --------------------------------------------------------------------------------
--
-- lpush :: Value a => Key -> a -> Tx Int
-- lpush key val = compareType key (returnInt ["LPUSH", key, en val]) (ListType  (typeOf val))
--
-- lpop :: Value a => Key -> Tx (Maybe a)
-- lpop key = compareCmdType key (returnMaybe ["LPOP", key]) (ListType  . carrier . deferred)
--
-- llen :: Key -> Tx Int
-- llen key = compareType key (returnInt ["LLEN", key]) ListOfAnything
--
-- lrange :: Value a => Key -> Integer -> Integer -> Tx [a]
-- lrange key m n = compareCmdType key (returnList  ["LRANGE", key, en m, en n]) (ListType  . carrier . deferred)
--
-- lindex :: Value a => Key -> Integer -> Tx (Maybe a)
-- lindex key n = compareCmdType key (returnMaybe ["LINDEX", key, en n]) (ListType  . carrier . deferred)
--
-- --------------------------------------------------------------------------------
-- --  Set
-- --------------------------------------------------------------------------------
--
-- sadd :: Value a => Key -> a -> Tx Status
-- sadd key val = compareType key (returnStatus ["SADD", key, en val]) (SetType  (typeOf val))
--
-- scard :: Key -> Tx Int
-- scard key = compareType key (returnInt ["SCARD", key]) SetOfAnything
--
-- smembers :: Value a => Key -> Tx [a]
-- smembers key = compareCmdType key (returnList ["SMEMBERS", key]) (SetType  . carrier . deferred)
--
-- spop :: Value a => Key -> Tx (Maybe a)
-- spop key = compareCmdType key (returnMaybe ["SPOP", key]) (SetType  . carrier . deferred)
