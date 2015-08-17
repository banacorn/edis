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

        declare "a" :: Tx Int

        -- set "a" (42 :: Int)
        incr "a"
        get "a" :: Tx (Deferred Int)

        -- set "foo" True
        -- set "bar" 'c'
        --
        -- foo <- get "foo" :: Tx (Deferred Bool)
        -- bar <- get "bar" :: Tx (Deferred Char)
        --
        -- return $ (,) <$> foo <*> bar

    case result of
        Left err -> print err
        Right ok -> print ok

    return ()
