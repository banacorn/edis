{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, OverloadedStrings, PolyKinds #-}

module Database.Edis.Promoted where

import Database.Edis.Type

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
