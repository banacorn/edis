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
    
        set "foo" True
        set "bar" 'c'

        foo <- get "foo" :: Tx (Deferred Bool)
        bar <- get "bar" :: Tx (Deferred Char)

        return $ (,) <$> foo <*> bar

    case result of
        Left err -> print err
        Right ok -> print ok

    return ()
