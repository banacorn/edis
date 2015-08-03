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


main :: IO ()
main = do
    conn <- connect defaultConnectInfo
    result <- R.runRedis conn $ do

        -- result <- R.multiExec $ do
        --     R.set "hello" "hey"
        --     R.get "hello"
        --     R.incr "hello"
        --     R.set "hello" "haha"
        --     R.get "hello"
        --
        -- liftIO $ print result

        T.execTransactions $ do
            T.set "a" "40"
            T.incr "a"



    return ()
