{-# LANGUAGE PolyKinds #-}

module Database.Edis.Command.Scripting where

import Database.Edis.Type

import Data.ByteString          (ByteString)
import Database.Redis as Redis hiding (decode)

--------------------------------------------------------------------------------
--  Scripting
--------------------------------------------------------------------------------

eval :: (RedisResult a)
     => ByteString -> [ByteString] -> [ByteString] -> Edis xs xs (Either Reply a)
eval script keys' args = Edis $ Redis.eval script keys' args

evalsha :: (RedisResult a)
     => ByteString -> [ByteString] -> [ByteString] -> Edis xs xs (Either Reply a)
evalsha script keys' args = Edis $ Redis.evalsha script keys' args

scriptExists :: [ByteString] -> Edis xs xs (Either Reply [Bool])
scriptExists scripts = Edis $ Redis.scriptExists scripts

scriptFlush :: Edis xs xs (Either Reply Status)
scriptFlush = Edis $ Redis.scriptFlush

scriptKill :: Edis xs xs (Either Reply Status)
scriptKill = Edis $ Redis.scriptFlush

scriptLoad :: ByteString -> Edis xs xs (Either Reply ByteString)
scriptLoad script = Edis $ Redis.scriptLoad script
