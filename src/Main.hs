{-# LANGUAGE OverloadedStrings, DeriveGeneric, DefaultSignatures, DeriveDataTypeable #-}

module Main where

import Tredis as T

import GHC.Generics
import Data.Typeable
import Database.Redis (connect, defaultConnectInfo)
import Data.ByteString
import Database.Redis as R
import Data.Serialize (Serialize)
import Control.Monad.Trans (liftIO)

main :: IO ()
main = do
    conn <- connect defaultConnectInfo

    -- type mismatch
    -- runRedis conn $ do
    --     R.set "hello" "hey"                 -- SET hello hey
    --     R.incr "hello"                      -- INCR hello
    --     R.get "hello" >>= liftIO . print    -- GET hello

    -- typed Redis
    -- result <- runTredis conn $ do
    --     T.set "hello" ("hjahskjfh" :: ByteString)
    --     T.incr "hello" -- >>= (liftIO . print :: TredisReply Integer       -> Tredis ())
    --     T.get "hello"  -- >>= (liftIO . print :: TredisReply (Maybe ByteString) -> Tredis ())
    -- print result

    -- typed Redis
    result <- runTredis conn $ do
        T.set "hello" ([1, 2, 3] :: [Int])
        T.get "hello"  >>= (liftIO . print :: TredisReply (Maybe [Int]) -> Tredis ())
        T.incr "hello" >>= (liftIO . print :: TredisReply Integer       -> Tredis ())
    -- print result

    return ()
