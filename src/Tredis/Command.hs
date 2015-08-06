{-# LANGUAGE OverloadedStrings #-}

module Tredis.Command where

import Tredis.Transaction
import Data.ByteString hiding (map, unpack, pack, reverse)
import Data.ByteString.Char8 (pack, unpack)
import Data.Serialize as S hiding (get, put)

--------------------------------------------------------------------------------
--  String
--------------------------------------------------------------------------------

declare :: Key -> Type -> Tx ()
declare = assertType

append :: Key -> ByteString -> Tx ()
append key val = do
    assertType key StrType
    insertCmd $ Append key val

get :: Key -> Tx ()
get key = insertCmd $ Get key

set :: Serialize a => Key -> a -> Tx ()
set key val = do
    assertType key StrType
    insertCmd $ Set key val


setInt :: Key -> Integer -> Tx ()
setInt key val = do
    assertType key IntType
    insertCmd $ Set key (pack (show val))


del :: Key -> Tx ()
del key = do
    removeType key
    insertCmd $ Del key

incr :: Key -> Tx ()
incr key = do
    checkType key IntType
    insertCmd $ Incr key
