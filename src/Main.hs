{-# LANGUAGE OverloadedStrings, DeriveGeneric, DefaultSignatures, DeriveDataTypeable #-}

module Main where

import Tredis as T

import GHC.Generics
import Data.Typeable
import Database.Redis (connect, defaultConnectInfo)
import Data.ByteString
import Data.Serialize (Serialize)
import Control.Monad.Trans (liftIO)

-- printL :: Show a => a -> Tredis ()
-- printL = liftIO . print

main :: IO ()
main = do
    conn <- connect defaultConnectInfo
    result <- runTredis conn $ do

        set "hello" [True, False, True]
        get "hello"  :: Tredis (RedisReply (Maybe [Bool]))
        -- incr "hello"


    print result
