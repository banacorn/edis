{-# LANGUAGE OverloadedStrings, DataKinds #-}

module Main where

import Database.Edis


main :: IO ()
main = do
    conn <- connect defaultConnectInfo
    result <- runRedis conn $ unEdis $ start
        >>> zadd    (Proxy :: Proxy "S")   3.0 True
        -- >>> smove   (Proxy :: Proxy "L")    (Proxy :: Proxy "Z")    True
        -- >>> smembers    (Proxy :: Proxy "S")
        -- >>> set         (Proxy :: Proxy "S")    True
        -- >>> incr        (Proxy :: Proxy "A")
    print result
    return ()
