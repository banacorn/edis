{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, OverloadedStrings, PolyKinds #-}

module Database.Edis.Command (
        module Database.Edis.Command.Key
    ,   module Database.Edis.Command.String
    ,   module Database.Edis.Command.List
    ,   module Database.Edis.Command.Hash
    ,   module Database.Edis.Command.Set
    ,   module Database.Edis.Command.ZSet
    ,   module Database.Edis.Command.Server
    ,   module Database.Edis.Command.Connection
    ,   module Database.Edis.Command.Scripting
    ,   module Database.Edis.Command.PubSub
    ,   start, declare, renounce
    ) where

import Database.Edis.Type
import Database.Edis.Command.Key
import Database.Edis.Command.String
import Database.Edis.Command.List
import Database.Edis.Command.Hash
import Database.Edis.Command.Set
import Database.Edis.Command.ZSet
import Database.Edis.Command.Server
import Database.Edis.Command.Connection
import Database.Edis.Command.Scripting
import Database.Edis.Command.PubSub

import Data.Proxy               (Proxy)
import GHC.TypeLits

--------------------------------------------------------------------------------
--  Helper functions
--------------------------------------------------------------------------------

start :: Edis '[] '[] ()
start = Edis (return ())

--------------------------------------------------------------------------------
--  Declaration
--------------------------------------------------------------------------------

declare :: (KnownSymbol s, Member xs s ~ 'False)
        => Proxy s -> Proxy x -> Edis xs (Set xs s x) ()
declare _ _ = Edis $ return ()

renounce :: (KnownSymbol s, Member xs s ~ 'True)
        => Proxy s -> Edis xs (Del xs s) ()
renounce _ = Edis $ return ()
