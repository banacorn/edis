{-# LANGUAGE OverloadedStrings #-}

module Tredis.Command where

import Tredis.Transaction
import Data.Typeable
import Data.ByteString hiding (map, unpack, pack, reverse)
import Data.ByteString.Char8 (pack, unpack)
import Data.Serialize as S hiding (get, put)

--------------------------------------------------------------------------------
--  String
--------------------------------------------------------------------------------

-- append :: Key -> ByteString -> Tx ()
-- append key val = do
--     assertType key StrType
--     insertCmd $ Append key val

get :: Key -> Tx ()
get key = insertCmd $ Get key

set :: (Serialize a, Typeable a) => Key -> a -> Tx ()
set key val = do
    assertType key val
    insertCmd $ Set key val

del :: Key -> Tx ()
del key = do
    removeType key
    insertCmd $ Del key

incr :: Key -> Tx ()
incr key = do
    checkType key (typeOf (1 :: Int))
    insertCmd $ Incr key
