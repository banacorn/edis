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

set :: (Serialize a, Typeable a) => Key -> a -> Tx (Queued a)
set key val = do
    assertType key val
    insertCmd ["SET", key, encode val]

del :: Serialize a => Key -> Tx (Queued a)
del key = do
    removeType key
    insertCmd ["DEL", key]

incr :: Serialize a => Key -> Tx (Queued a)
incr key = do
    checkType key (typeOf (0 :: Int))
    insertCmd ["INCR", key]
