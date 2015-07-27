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

main :: IO ()
main = do
    conn <- connect defaultConnectInfo
    runTredis conn $ do
        set "hello" ([R, G, Y] :: [L]) >>= liftIO . print
        incr "hello"
        get "hello" >>= liftIO . (print :: Either TredisReply (Maybe [L]) -> IO ())

        -- set "hello" True >>= liftIO . print
        -- a <- get "hello" :: Tredis (Either TredisReply (Maybe Bool))
        -- liftIO $ print a


        -- incr "hello" >>= liftIO . print
        -- a <- get "hello" >>= decode :: Redis (Either E L)
        -- liftIO $ print a

        -- set "hello" $ encode (Just 2 :: String)
        -- a <- get "hello"
        -- a <- get "hello" >>= decode :: Redis (Either E Bool)
        -- liftIO $ print a
