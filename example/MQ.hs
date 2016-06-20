{-# LANGUAGE OverloadedStrings, DataKinds, DeriveGeneric, TypeFamilies #-}

module MQ where

import Database.Edis
import Data.ByteString
import Data.Serialize hiding (Get)
import GHC.Generics

data Message = Msg ByteString Integer deriving (Show, Generic)
instance Serialize Message where

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight (Left _) = error "Left"

push :: (StringOfIntegerOrNX xs "id", ListOrNX xs "queue")
    => ByteString
    -> Edis xs (Set xs "queue" (ListOf Message)) (Either Reply Integer)
push msg =          incr    (Proxy :: Proxy "id")
    `bind` \i ->    lpush   (Proxy :: Proxy "queue")    (Msg msg (fromRight i))

pop :: (Get xs "queue" ~ 'Just (ListOf Message))
    => Edis xs xs (Either Reply (Maybe Message))
pop = rpop (Proxy :: Proxy "queue")

-- main :: IO ()
-- main = do
--     conn <- connect defaultConnectInfo
--     result <- runRedis conn $ unEdis $ start
--         >>> push "hello"
--         >>> pop
--
--     print $ result
--     return ()

main :: IO ()
main = do conn <- connect defaultConnectInfo
          result <- runRedis conn $ unEdis $ (start >>> prog)
          print $ result
          return ()

prog :: Edis
       '[]
       '[ '("id", Integer), '("queue", ListOf Message) ]
       (Either Reply (Maybe Message))
prog =  declare kId (Proxy :: Proxy Integer)
    >>> declare kQueue (Proxy :: Proxy (ListOf Message))
    >>> push "hello"
    >>> push "world"
    >>> pop
    where
        kId :: Proxy "id"
        kId = Proxy

        kQueue :: Proxy "queue"
        kQueue = Proxy
