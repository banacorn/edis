module Database.Edis.Promoted.Connection where

import Database.Edis.Type

import Data.ByteString          (ByteString)
import Database.Redis as Redis hiding (decode)

--------------------------------------------------------------------------------
--  Connection
--------------------------------------------------------------------------------

auth :: ByteString -> Edis xs xs (Either Reply Status)
auth = Edis . Redis.auth

echo :: ByteString -> Edis xs xs (Either Reply ByteString)
echo = Edis . Redis.echo

ping :: Edis xs xs (Either Reply Status)
ping = Edis Redis.ping

quit :: Edis xs xs (Either Reply Status)
quit = Edis Redis.quit

select :: Integer -> Edis xs xs (Either Reply Status)
select = Edis . Redis.select
