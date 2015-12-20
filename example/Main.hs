{-# LANGUAGE OverloadedStrings, DataKinds #-}

module Main where

import Database.Edis


main :: IO ()
main = do
    conn <- connect defaultConnectInfo
    result <- runRedis conn $ unEdis $ start
        >>> lpush   (Proxy :: Proxy "L")    True
        >>> sadd    (Proxy :: Proxy "S")    True
        >>> sadd    (Proxy :: Proxy "S")    False
        >>> sadd    (Proxy :: Proxy "T")    False
        >>> sinter  (Proxy :: Proxy "S")    (Proxy :: Proxy "T")
        -- >>> smove   (Proxy :: Proxy "L")    (Proxy :: Proxy "Z")    True
        -- >>> smembers    (Proxy :: Proxy "S")
        -- >>> set         (Proxy :: Proxy "S")    True
        -- >>> incr        (Proxy :: Proxy "A")
    print result
    return ()
