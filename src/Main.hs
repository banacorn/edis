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

        del "a"
        declare "a"     :: Tx [Bool]
        -- set "a" False                       -- type error!
        -- get "a"         :: Tx (Queued Bool) -- type error!
        lpush "a" True
        lpush "a" False
        lpop "a" :: Tx (Queued Int)

    case result of
        Left err -> print err
        Right ok -> print ok

    return ()
