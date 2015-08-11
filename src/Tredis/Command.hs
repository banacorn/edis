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
    let Right val = decode ""
    assertTypeVal key val
    return val

get :: (Serialize a, Typeable a) => Key -> Tx (Queued a)
get key = do
    queued <- insertCmd ["GET", key]
    let queuedType = head (typeRepArgs (typeOf queued))
    typeError <- checkType key queuedType
    case typeError of
        Just er -> do
            assertError er
            return $ error (show er)
        Nothing -> return queued

set :: (Serialize a, Typeable a) => Key -> a -> Tx (Queued ())
set key val = do
    assertTypeVal key val
    insertCmd ["SET", key, encode val]

del :: Key -> Tx (Queued ())
del key = do
    removeType key
    insertCmd ["DEL", key]

incr :: Key -> Tx (Queued ())
incr key = do
    checkType key (typeOf (0 :: Int))
    insertCmd ["INCR", key]

--------------------------------------------------------------------------------
--  List
--------------------------------------------------------------------------------

lpush :: (Serialize a, Typeable a) => Key -> a -> Tx (Queued ())
lpush key val = do
    assertTypeVal key [val]
    insertCmd ["LPUSH", key, encode val]

lpop :: (Serialize a, Typeable a) => Key -> Tx (Queued a)
lpop key = do
    -- force instantiate the type of queued result
    queued <- insertCmd ["LPOP", key]
    let queuedType = head (typeRepArgs (typeOf queued))

    typRep <- lookupType key
    case typRep of
        Just ty -> do
            let (conType, argType) = splitTyConApp ty
            if conType == typeRepTyCon (typeOf [True]) && head argType == queuedType
                then return queued
                else do
                    assertError (TypeMismatch key (mkTyConApp conType argType) queuedType)
                    return $ error "TypeMismatch"
        Nothing -> do
            assertError (Undeclared key)
            return $ error "Undeclared"

-- fixType :: a -> a
