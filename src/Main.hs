{-# LANGUAGE OverloadedStrings, DeriveGeneric, DefaultSignatures, DeriveDataTypeable #-}

module Main where

-- import Tredis as T

import Tredis.Transaction
import Tredis.Command

import GHC.Generics
import Data.Typeable
import Database.Redis (connect, defaultConnectInfo, runRedis)
import Data.ByteString
import Data.Serialize (Serialize)
import Control.Monad.Trans (liftIO)
import Control.Applicative

import Data.Int (Int64)
main :: IO ()
main = do
    conn <- connect defaultConnectInfo
    result <- runRedis conn $ do

        reply <- runTx $ do
            set "a" True
            get "a"
            del "a"

        case reply of
            Right rs -> liftIO $ print rs
            Left err -> liftIO $ print err

    return ()
