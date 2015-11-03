{-# LANGUAGE OverloadedStrings, DataKinds #-}

module Main where

import Data.ByteString

import Edis

program = start
    `bind` \_ -> ping
    `bind` \_ -> set      (Proxy :: Proxy "A") True
    `bind` \_ -> set      (Proxy :: Proxy "A") 1
    `bind` \_ -> incr     (Proxy :: Proxy "A")
    `bind` \_ -> lpush    (Proxy :: Proxy "L") 'a'
    `bind` \_ -> lpop     (Proxy :: Proxy "L")
    `bind` \a -> lpush     (Proxy :: Proxy "L") (fromMaybeResult a)
    `bind` \_ -> lpush     (Proxy :: Proxy "L") (fromMaybeResult a)
    `bind` \_ -> llen     (Proxy :: Proxy "L")

program0 = start
    >>> ping
    >>> set     (Proxy :: Proxy "A") True
    >>> set     (Proxy :: Proxy "A") 1
    >>> incr    (Proxy :: Proxy "A")
    >>> lpush   (Proxy :: Proxy "L") 'a'
    >>> lpush   (Proxy :: Proxy "L") 'b'
    >>> decr    (Proxy :: Proxy "A")
    >>> lpop    (Proxy :: Proxy "L")
    >>> llen    (Proxy :: Proxy "L")

main :: IO ()
main = do
    conn <- connect defaultConnectInfo
    result <- runRedis conn $ unP $ start
        >>> set     (Proxy :: Proxy "A") 1
        >>> incr    (Proxy :: Proxy "A")
        >>> lpush   (Proxy :: Proxy "L") 'a'
        >>> lpush   (Proxy :: Proxy "L") 'b'
        >>> decr    (Proxy :: Proxy "A")
        >>> lpop    (Proxy :: Proxy "L")
        >>> llen    (Proxy :: Proxy "L")
    print result
    return ()
