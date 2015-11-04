{-# LANGUAGE OverloadedStrings, DataKinds #-}

module Main where

import Data.ByteString (ByteString)

import Edis

-- program = start
--     `bind` \_ -> ping
--     `bind` \_ -> set      (Proxy :: Proxy "A") True
--     `bind` \_ -> set      (Proxy :: Proxy "A") (1 :: Integer)
--     `bind` \_ -> incr     (Proxy :: Proxy "A")
--     `bind` \_ -> lpush    (Proxy :: Proxy "L") 'a'
--     `bind` \_ -> lpop     (Proxy :: Proxy "L")
--     `bind` \a -> lpush     (Proxy :: Proxy "L") (fromMaybeResult a)
--     `bind` \_ -> lpush     (Proxy :: Proxy "L") (fromMaybeResult a)
--     `bind` \_ -> llen     (Proxy :: Proxy "L")
--
-- program0 = start
--     >>> ping
--     >>> set     (Proxy :: Proxy "A") True
--     >>> set     (Proxy :: Proxy "A") (1 :: Integer)
--     -- >>> incr    (Proxy :: Proxy "A")
--     >>> lpush   (Proxy :: Proxy "L") 'a'
--     >>> lpush   (Proxy :: Proxy "L") 'b'
--     >>> decr    (Proxy :: Proxy "A")
--     >>> lpop    (Proxy :: Proxy "L")
--     >>> llen    (Proxy :: Proxy "L")

main :: IO ()
main = do
    conn <- connect defaultConnectInfo
    result <- runRedis conn $ unP $ start
        -- >>> del     (Proxy :: Proxy "H")
        -- >>> hset   (Proxy :: Proxy "H") (Proxy :: Proxy "a") ()
        >>> hincrby   (Proxy :: Proxy "H") (Proxy :: Proxy "a") 42
        >>> hsetnx    (Proxy :: Proxy "H") (Proxy :: Proxy "b") (0 :: Integer)
        -- >>> hget        (Proxy :: Proxy "H") (Proxy :: Proxy "a")
        -- >>> declare     (Proxy :: Proxy "A") (Proxy :: Proxy Integer)
        -- >>> set         (Proxy :: Proxy "A") (3 :: Integer)
        -- >>> del     (Proxy :: Proxy "A")
        -- >>> declare (Proxy :: Proxy "A") (Proxy :: Proxy Int)
        -- >>> set         (Proxy :: Proxy "B") True
        -- >>> renamenx    (Proxy :: Proxy "A")  (Proxy :: Proxy "C")
        -- >>> keys "*"
        -- >>> incr     (Proxy :: Proxy "B")
        -- >>> bitcount  "A"
        -- >>> append  (Proxy :: Proxy "A") ("heeee" :: ByteString)
        -- >>> get     (Proxy :: Proxy "A")
    print result
    return ()
