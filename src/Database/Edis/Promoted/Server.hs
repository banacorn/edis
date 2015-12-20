module Database.Edis.Promoted.Server where

import Database.Edis.Type

import Data.ByteString          (ByteString)
import Database.Redis as Redis hiding (decode)

--------------------------------------------------------------------------------
--  Server
--------------------------------------------------------------------------------

bgrewriteaof :: Edis xs xs (Either Reply Status)
bgrewriteaof = Edis $ Redis.bgrewriteaof

bgsave :: Edis xs xs (Either Reply Status)
bgsave = Edis $ Redis.bgsave

configGet :: ByteString -> Edis xs xs (Either Reply [(ByteString, ByteString)])
configGet param = Edis $ Redis.configGet param

configResetstat :: Edis xs xs (Either Reply Status)
configResetstat = Edis $ Redis.configResetstat

configSet :: ByteString -> ByteString -> Edis xs xs (Either Reply Status)
configSet param val = Edis $ Redis.configSet param val

dbsize :: Edis xs xs (Either Reply Integer)
dbsize = Edis $ Redis.dbsize

debugObject :: ByteString -> Edis xs xs (Either Reply ByteString)
debugObject key = Edis $ Redis.debugObject key

flushall :: Edis xs xs (Either Reply Status)
flushall = Edis $ Redis.flushall

flushdb :: Edis xs xs (Either Reply Status)
flushdb = Edis $ Redis.flushdb

info :: Edis xs xs (Either Reply ByteString)
info = Edis $ Redis.info

lastsave :: Edis xs xs (Either Reply Integer)
lastsave = Edis $ Redis.lastsave

save :: Edis xs xs (Either Reply Status)
save = Edis $ Redis.save

slaveof :: ByteString -> ByteString -> Edis xs xs (Either Reply Status)
slaveof host port = Edis $ Redis.slaveof host port

slowlogGet :: Integer -> Edis xs xs (Either Reply [Slowlog])
slowlogGet count = Edis $ Redis.slowlogGet count

slowlogLen :: Edis xs xs (Either Reply Integer)
slowlogLen = Edis $ Redis.slowlogLen

slowlogReset :: Edis xs xs (Either Reply Status)
slowlogReset = Edis $ Redis.slowlogReset

time :: Edis xs xs (Either Reply (Integer, Integer))
time = Edis $ Redis.time
