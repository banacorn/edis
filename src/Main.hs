{-# LANGUAGE OverloadedStrings, DeriveGeneric, DefaultSignatures, DeriveDataTypeable #-}

module Main where

-- import Tredis as T

import Tredis.Transaction as T

import GHC.Generics
import Data.Typeable
import Database.Redis (connect, defaultConnectInfo)
import Data.ByteString
import Database.Redis as R
import Data.Serialize (Serialize)
import Control.Monad.Trans (liftIO)
import Control.Applicative

import Data.Int (Int64)
main :: IO ()
main = do
    conn <- connect defaultConnectInfo
    result <- R.runRedis conn $ do

        T.execTransactions $ do
            T.set "a" "40"
            T.incr "a"
            T.get "a"



    return ()
