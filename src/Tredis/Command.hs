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
    insertType (Sing key) (typeToInsert val)
    return $ Deferred $ \_ -> Right val
    where   typeToInsert val
                | tyCon == listTyCon = ListType $ head tyArgs
                | tyCon == setTyCon = SetType $ head tyArgs
                | tyCon == hashTyCon = HashType
                | otherwise = Type $ typeOf val
                where   (tyCon, tyArgs) = splitTyConApp (typeOf val)
                        listTyCon = typeRepTyCon listTypeRep
                        setTyCon = typeRepTyCon setTypeRep
                        hashTyCon = typeRepTyCon hashTypeRep

declareField :: Value a => Key -> Field -> Tx a
declareField key field = do
    let Right val = de "witchcraft" -- fake a value
    insertType (Pair key field) (typeToInsert val)
    return $ Deferred $ \_ -> Right val
    where   typeToInsert val
                | tyCon == listTyCon = ListType $ head tyArgs
                | tyCon == setTyCon = SetType $ head tyArgs
                | tyCon == hashTyCon = HashType
                | otherwise = Type $ typeOf val
                where   (tyCon, tyArgs) = splitTyConApp (typeOf val)
                        listTyCon = typeRepTyCon listTypeRep
                        setTyCon = typeRepTyCon setTypeRep
                        hashTyCon = typeRepTyCon hashTypeRep


set :: Value a => Key -> a -> Tx Status
set key val = compareType (Sing key) (returnStatus (SET key val)) (Type (typeOf val))

incr :: Key -> Tx Int
incr key = compareType (Sing key) (returnInt (INCR key)) (Type intTypeRep)

decr :: Key -> Tx Int
decr key = compareType (Sing key) (returnInt (DECR key)) (Type intTypeRep)

get :: Value a => Key -> Tx (Maybe a)
get key = compareResult (Sing key) (returnMaybe (GET key)) (Type . carrier)

del :: Key -> Tx Status
del key = do
    removeType (Sing key)
    returnStatus (DEL key)

--------------------------------------------------------------------------------
--  List
--------------------------------------------------------------------------------

lpush :: Value a => Key -> a -> Tx Int
lpush key val = compareType (Sing key) (returnInt (LPUSH key val)) (ListType (typeOf val))

lpop :: Value a => Key -> Tx (Maybe a)
lpop key = compareResult (Sing key) (returnMaybe (LPOP key)) (ListType . carrier)

llen :: Key -> Tx Int
llen key = compareType (Sing key) (returnInt (LLEN key)) ListOfAnything

lrange :: Value a => Key -> Integer -> Integer -> Tx [a]
lrange key m n = compareResult (Sing key) (returnList  (LRANGE key m n)) (ListType . carrier)

lindex :: Value a => Key -> Integer -> Tx (Maybe a)
lindex key n = compareResult (Sing key) (returnMaybe (LINDEX key n)) (ListType . carrier)

--------------------------------------------------------------------------------
--  Set
--------------------------------------------------------------------------------

sadd :: Value a => Key -> a -> Tx Status
sadd key val = compareType (Sing key) (returnStatus (SADD key val)) (SetType (typeOf val))

srem :: Key -> Tx Int
srem key = compareType (Sing key) (returnInt (SREM key)) SetOfAnything

scard :: Key -> Tx Int
scard key = compareType (Sing key) (returnInt (SCARD key)) SetOfAnything

smembers :: Value a => Key -> Tx [a]
smembers key = compareResult (Sing key) (returnList (SMEMBERS key)) (SetType . carrier)

spop :: Value a => Key -> Tx (Maybe a)
spop key = compareResult (Sing key) (returnMaybe (SPOP key)) (SetType . carrier)

--------------------------------------------------------------------------------
--  Hash
--------------------------------------------------------------------------------

hset :: Value a => Key -> Field -> a -> Tx Bool
hset key field val = compareType (Pair key field) (returnBool (HSET key field val)) (Type (typeOf val))
