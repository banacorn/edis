{-# LANGUAGE OverloadedStrings, DeriveGeneric, DefaultSignatures #-}

module Main where

import Tredis

import GHC.Generics
import Database.Redis hiding (decode)
import Data.ByteString
import Data.Serialize (Serialize)
import Control.Monad.Trans (liftIO)

data L = R | G | Y deriving (Show, Generic)
instance Serialize L

main :: IO ()
main = do
    conn <- connect defaultConnectInfo
    runRedis conn $ do
        set "hello" (encode [R, G, G, Y])
        a <- get "hello" >>= decode :: Redis (Either E L)
        liftIO $ print a
