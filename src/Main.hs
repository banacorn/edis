{-# LANGUAGE OverloadedStrings, DataKinds #-}

module Main where

import Data.ByteString

import Edis

program = start
    `bind` \_ -> ping
    -- `bind` \_ -> set      (Proxy :: Proxy "A") True
    -- `bind` \_ -> set      (Proxy :: Proxy "A") 1
    -- `bind` \_ -> incr     (Proxy :: Proxy "A")
    -- `bind` \_ -> lpush    (Proxy :: Proxy "L") 'a'
    -- `bind` \_ -> lpop     (Proxy :: Proxy "L")
    -- `bind` \a -> sadd     (Proxy :: Proxy "S") 'z'
    -- `bind` \_ -> srem     (Proxy :: Proxy "S") 'c'
    `bind` \_ -> dec     (Proxy :: Proxy "L") (Proxy :: Proxy (ListK Char))
    `bind` \_ -> lrange  (Proxy :: Proxy "L")  0 4

main :: IO ()
main = do
    conn <- connect defaultConnectInfo
    runRedis conn (unP program) >>= print

    return ()
