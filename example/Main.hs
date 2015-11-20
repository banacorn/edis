{-# LANGUAGE OverloadedStrings, DataKinds #-}

module Main where

import Data.ByteString (ByteString)
import Edis

main :: IO ()
main = do
    conn <- connect defaultConnectInfo
    result <- runRedis conn $ unEdis $ start
        `bind` \_ ->  declare   (Proxy :: Proxy "A") (Proxy :: Proxy Integer)
        `bind` \_ ->  incr      (Proxy :: Proxy "A")
        `bind` \n ->  case n of
            Left  reply -> error (show reply)
            Right n     -> lpush (Proxy :: Proxy "L") n
        `bind` \_ ->  lpop      (Proxy :: Proxy "L")
    print result
    return ()
