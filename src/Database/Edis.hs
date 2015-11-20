module Database.Edis (
        module Database.Edis.Promoted
    ,   Proxy(..)
    ,   Edis(..)
    ,   IMonad(..)
    ,   ListOf(..)
    ,   SetOf(..)
    ,   HashOf(..)
    ,   (>>>)

    -- ,   Status(..)
    ,   Redis.Reply
    ,   Redis.runRedis
    ,   Redis.connect
    ,   Redis.defaultConnectInfo
    ) where

import qualified Database.Redis as Redis
import Data.Proxy
import Database.Edis.Type
import Database.Edis.Promoted
