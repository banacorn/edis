{-# LANGUAGE OverloadedStrings, DeriveGeneric, DefaultSignatures, DeriveDataTypeable #-}

module Main where

import Tredis as T

import GHC.Generics
import Data.Typeable
import Database.Redis (connect, defaultConnectInfo)
import Data.ByteString
import Data.Serialize (Serialize)
import Control.Monad.Trans (liftIO)

data L = R | G | Y deriving (Show, Generic, Typeable)
instance Serialize L


printL :: Show a => a -> Tredis ()
printL = liftIO . print

main :: IO ()
main = do
    conn <- connect defaultConnectInfo
    runTredis conn $ do

        set "hello" [True, False, True]

        a <- get "hello" :: Tredis (Either TredisReply (Maybe [Bool]))
        printL a


        incr "hello" >>= printL

        return ()
