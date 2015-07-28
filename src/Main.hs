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

    -- original Redis
    runRedis conn $ do
        R.set "hello" "hey"
        R.get "hello"  >>= (liftIO . print :: Either R.Reply (Maybe ByteString) -> Redis ())
        R.incr "hello" >>= (liftIO . print :: Either R.Reply Integer            -> Redis ())

    -- typed Redis
    -- result <- runTredis conn $ do
    --     T.set "hello" ([1, 2, 3] :: [Int])
    --     T.get "hello"  >>= (liftIO . print :: TredisReply (Maybe [Int]) -> Tredis ())
    --     T.incr "hello" >>= (liftIO . print :: TredisReply Integer       -> Tredis ())
    -- -- print result

    return ()
