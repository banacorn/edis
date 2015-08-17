{-# LANGUAGE OverloadedStrings, DeriveGeneric, DefaultSignatures, DeriveDataTypeable #-}

module Main where

import Tredis.Transaction
import Tredis.Command

import Data.Serialize (Serialize)
import Control.Applicative

main :: IO ()
main = do
    conn <- connect defaultConnectInfo
    result <- runTx' conn $ do


        -- del "a"
        -- declare "a" :: Tx' [Int]
        -- lpush "a" (3 :: Int)
        -- lpop "a" :: Tx' (Deferred (Maybe Int))
        -- lrange "a" 0 (-2) :: Tx' (Deferred [Int])
        -- llen "a"
        -- lindex "a" 2 :: Tx' (Deferred (Maybe Int))

        -- set "a" (42 :: Int)
        -- incr "a"
        -- get "a" :: Tx' (Deferred (Maybe Int))

        sadd "set" (4 :: Int)
        sadd "set" (1 :: Int)
        sadd "set" (2 :: Int)
        sadd "set" (10 :: Int)
        sadd "set" (2 :: Int)
        -- sadd "set" (4 :: Int)
        scard "set"


    case result of
        Left err -> print err
        Right ok -> print ok

    return ()
