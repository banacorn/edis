{-# LANGUAGE OverloadedStrings, DeriveGeneric, DefaultSignatures, DeriveDataTypeable #-}

module Main where

-- import Tredis as T

import Tredis.Transaction as T

import GHC.Generics
import Data.Typeable
import Database.Redis (connect, defaultConnectInfo, runRedis)
import Data.ByteString
import Database.Redis as R
import Data.Serialize (Serialize)
import Control.Monad.Trans (liftIO)
import Control.Applicative

import Data.Int (Int64)
main :: IO ()
main = do
    conn <- connect defaultConnectInfo
    result <- runRedis conn $ do

        reply <- T.runTx $ do
            T.setInt "a" 40
            T.set "a" "haha"
            T.incr "a"
            T.get "a"
            T.del "a"
            T.set "a" "haha"
            T.get "a"

        case reply of
            Right rs -> liftIO $ print rs
            Left err -> liftIO $ print err

    return ()
