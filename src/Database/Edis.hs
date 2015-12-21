module Database.Edis (
        module Database.Edis.Command
    ,   Proxy(..)
    ,   Edis(..)
    ,   IMonad(..)
    ,   (>>>)

    ,   Redis.Reply
    ,   Redis.runRedis
    ,   Redis.connect
    ,   Redis.defaultConnectInfo
    ) where

import           Data.Proxy
import qualified Database.Redis as Redis
import           Database.Edis.Type
import           Database.Edis.Command
