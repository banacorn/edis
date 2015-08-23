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

        -- ping
        -- declare "a" :: Tx Int
        -- incr "a"

        declare "list" :: Tx (List Int)
        lpush "list" (3 :: Int)
        lpop "list" :: Tx (Maybe Int)
        lrange "list" 0 (-2) :: Tx [Int]
        -- llen "list"
        lindex "list" 2 :: Tx (Maybe Int)

        -- -- string
        -- declare "n" :: Tx Int
        -- set "n" (42 :: Int)
        -- incr "n"
        -- get "n" :: Tx' (Deferred (Maybe Int))

        -- declare "set" :: Tx (Set Int)
        -- sadd "set" (4 :: Int)
        -- sadd "set" (1 :: Int)
        -- sadd "set" (2:: Double)
        -- scard "set"
        -- smembers "set"


    case result of
        Left err -> print err
        Right ok -> print ok

    return ()
