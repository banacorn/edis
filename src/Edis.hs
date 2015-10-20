module Edis (
        module Edis.Command
    ,   execTx
    ,   checkTx
    ,   runTx
    ,   Tx
    ,   List(..)
    ,   Set(..)
    ,   Hash(..)
    ,   Status(..)
    ,   Redis.connect
    ,   Redis.defaultConnectInfo
    ) where

import qualified Database.Redis as Redis
import Edis.Command
import Edis.Transaction
import Edis.Type
