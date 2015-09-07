{-# LANGUAGE OverloadedStrings #-}

module Main where

import Tredis

program :: Tx Status
program = do

    ping


    -- declare "string" :: Tx Int
    -- set "string" (42 :: Int)
    -- incr "string"
    -- get "string" :: Tx (Maybe Int)

    -- declare "list" :: Tx (List Int)
    -- lpush "list" (3 :: Int)
    -- lpop "list" :: Tx (Maybe Int)
    -- lrange "list" 0 (-2) :: Tx [Int]
    -- llen "list"
    -- lindex "list" 2 :: Tx (Maybe Int)

    --
    -- declare "set" :: Tx (Set Int)
    -- sadd "set" (4 :: Int)
    -- sadd "set" (1 :: Int)
    -- sadd "set" (2:: Int)
    -- scard "set"
    -- smembers "set" :: Tx [Int]
    -- spop "set" :: Tx (Maybe Int)


main :: IO ()
main = do
    conn <- connect defaultConnectInfo

    --
    -- print (checkTx program)
    -- execTx conn program >>= print

    -- check + execute
    result <- runTx conn program
    case result of
        Left err -> print err
        Right ok -> print ok

    return ()
