module Database.Edis (
        -- * How to use Edis
        -- |It's basically the same as in <https://hackage.haskell.org/package/hedis Hedis>.
        --  Most of the entities in Hedis are re-exported, or have the same names
        --  with types.
        --
        -- @
        -- -- connects to localhost:6379
        -- conn <- 'connect' 'defaultConnectInfo'
        -- @
        --
        -- Send commands to the server:
        --
        -- @
        -- {-\# LANGUAGE OverloadedStrings \#-}
        -- ...
        -- 'runRedis' conn $ 'unEdis' $ 'start'
        --      '`bind`' \_ -> 'set'     ('Proxy' :: 'Proxy' \"hello\")    True
        --      '`bind`' \_ -> 'set'     ('Proxy' :: 'Proxy' \"world\")    [True, False]
        --      '`bind`' \_ -> 'get'     ('Proxy' :: 'Proxy' \"world\")
        -- @
        --
        -- Unfortunately 'Edis' is not a monad, so we can't use do-notations in
        -- the program. But the example above can be rewritten with '>>>' like
        -- this, if no variable bindings and result passings are needed.
        --
        -- @
        -- {-\# LANGUAGE OverloadedStrings \#-}
        -- ...
        -- 'runRedis' conn $ 'unEdis' $ 'start'
        --      '>>>' 'set'     ('Proxy' :: 'Proxy' \"hello\")    True
        --      '>>>' 'set'     ('Proxy' :: 'Proxy' \"world\")    [True, False]
        --      '>>>' 'get'     ('Proxy' :: 'Proxy' \"world\")
        -- @
        --
        -- * Commands
        module Database.Edis.Command
    ,   Proxy(..)
    ,   Edis(..)
    ,   IMonad(..)
    ,   (>>>)

    ,   StringOf
    ,   HashOf
    ,   ListOf
    ,   SetOf
    ,   ZSetOf

    ,   Redis.Reply
    ,   Redis.runRedis
    ,   Redis.connect
    ,   Redis.defaultConnectInfo
    ) where

import           Data.Proxy
import qualified Database.Redis as Redis
import           Database.Edis.Type
import           Database.Edis.Command
