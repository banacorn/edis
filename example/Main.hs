{-# LANGUAGE OverloadedStrings, DataKinds #-}

module Main where

import Database.Edis

main :: IO ()
main = do
    conn <- connect defaultConnectInfo
    runRedis conn $ unEdis $ start
        `bind`  \_ -> set     (Proxy :: Proxy "hello")    True
        `bind`  \_ -> set     (Proxy :: Proxy "world")    [True, False]
        `bind`  \_ -> get     (Proxy :: Proxy "world")
    return ()
