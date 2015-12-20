module Database.Edis.Promoted.PubSub (
        Database.Edis.Promoted.PubSub.publish
    ,   Database.Edis.Promoted.PubSub.pubSub
    ,   subscribe
    ,   unsubscribe
    ,   psubscribe
    ,   punsubscribe
) where

import Database.Edis.Type

import Data.ByteString          (ByteString)
import Database.Redis as Redis hiding (decode)

--------------------------------------------------------------------------------
--  Pub/Sub
--------------------------------------------------------------------------------

publish :: ByteString -> ByteString -> Edis xs xs (Either Reply Integer)
publish channel message = Edis $ Redis.publish channel message

pubSub :: PubSub -> (Message -> IO PubSub) -> Edis xs xs ()
pubSub initSub callback = Edis $ Redis.pubSub initSub callback
