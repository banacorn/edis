{-# LANGUAGE OverloadedStrings, DeriveGeneric, DefaultSignatures, DeriveDataTypeable #-}

module Main where

import Tredis.Transaction
import Tredis.Command

import Data.Serialize (Serialize)
import Control.Applicative

main :: IO ()
main = do
    conn <- connect defaultConnectInfo
    result <- runTx conn $ do
        -- set "a" True
        declare "a" :: Tx Int
        -- return ()
        -- get "a" :: Tx (Queued Int)
        -- del "a"
        lpush "a" False
        lpop "a" :: Tx (Queued Bool)

    case result of
        Left err -> print err
        Right ok -> print ok

    return ()
