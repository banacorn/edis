{-# LANGUAGE OverloadedStrings, DeriveGeneric, DefaultSignatures, DeriveDataTypeable #-}

module Main where

-- import Tredis as T

import Tredis.Transaction
import Tredis.Command

import Data.Serialize (Serialize)
-- import Control.Monad.Trans (liftIO)
import Control.Applicative

import Data.Int (Int64)
main :: IO ()
main = do
    conn <- connect defaultConnectInfo
    result <- runTx conn $ do
        set "a" (5 :: Int)
        lpush "a" True
        -- lpop "a" :: Tx (Queued Bool)
        -- incr "a" :: Tx (Queued Int)
        -- a <- get "a" :: Tx (Queued Int)
        -- incr "a" :: Tx (Queued Int)
        -- set "a" (6 :: Int)
        -- a' <- get "a" :: Tx (Queued Int)

    case result of
        Left err -> print err
        Right ok -> print ok

    return ()
