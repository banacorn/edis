{-# LANGUAGE OverloadedStrings #-}

module Tredis.Command where

import Tredis.Transaction
import Data.Typeable
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 (pack, unpack)
import           Data.Serialize (Serialize)

--------------------------------------------------------------------------------
--  String
--------------------------------------------------------------------------------

-- append :: Key -> ByteString -> Tx ()
-- append key val = do
--     assertType key StrType
--     insertCmd $ Append key val

get :: Serialize a => Key -> Tx (Queued a)
get key = insertCmd $ Get key

set :: (Serialize a, Typeable a) => Key -> a -> Tx (Queued a)
set key val = do
    assertType key val
    insertCmd $ Set key val

del :: Serialize a => Key -> Tx (Queued a)
del key = do
    removeType key
    insertCmd $ Del key

incr :: Serialize a => Key -> Tx (Queued a)
incr key = do
    checkType key (typeOf (0 :: Int))
    insertCmd $ Incr key
