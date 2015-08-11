{-# LANGUAGE OverloadedStrings, DeriveGeneric, DefaultSignatures, DeriveDataTypeable #-}

module Main where

-- import Tredis as T

import Tredis.Transaction
import Tredis.Command

import GHC.Generics
import Data.Typeable
import Database.Redis (connect, defaultConnectInfo, runRedis)
-- import Data.ByteString
import Data.Serialize (Serialize)
import Control.Monad.Trans (liftIO)
import Control.Applicative

import Data.Int (Int64)
main :: IO ()
main = do
    conn <- connect defaultConnectInfo
    result <- runTx conn $ do
        -- b <- set "a" (5 :: Int)
        a <- get "a" :: Tx (Queued Int)
        -- incr "a" :: Tx (Queued Int)
        -- set "a" (6 :: Int)
        -- a' <- get "a" :: Tx (Queued Int)
        return $ (,) <$> a <*> a

    case result of
        Left err -> liftIO $ print err
        Right ok -> liftIO $ print ok

    return ()
