{-# LANGUAGE OverloadedStrings #-}

module Tredis.Command where

import Tredis.Transaction
import Data.Typeable
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 (pack, unpack)
import           Data.Serialize (Serialize, encode)
import           Database.Redis (sendRequest)

--------------------------------------------------------------------------------
--  String
--------------------------------------------------------------------------------

append :: Serialize a => Key -> ByteString -> Tx (Queued a)
append key val = do
    checkType key (typeOf ("" :: ByteString))
    insertCmd ["APPEND", key, val]

get :: Serialize a => Key -> Tx (Queued a)
get key = insertCmd ["GET", key]

set :: (Serialize a, Typeable a) => Key -> a -> Tx (Queued ())
set key val = do
    assertType key val
    insertCmd ["SET", key, encode val]

del :: Key -> Tx (Queued ())
del key = do
    removeType key
    insertCmd ["DEL", key]

incr :: Key -> Tx (Queued ())
incr key = do
    checkType key (typeOf (0 :: Int))
    insertCmd ["INCR", key]

lpush :: (Serialize a, Typeable a) => Key -> a -> Tx (Queued ())
lpush key val = do
    assertType key [val]
    insertCmd ["LPUSH", key, encode val]

lpop :: Serialize a => Key -> Tx (Queued a)
lpop key = insertCmd ["LPOP", key]
