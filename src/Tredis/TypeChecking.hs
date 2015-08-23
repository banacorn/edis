{-# LANGUAGE GADTs, OverloadedStrings, DeriveGeneric, DeriveDataTypeable #-}

module Tredis.TypeChecking where

import           Tredis.Transaction
import           Data.Typeable
import           GHC.Generics

--------------------------------------------------------------------------------
--  type checking stuffs
--------------------------------------------------------------------------------

-- data Type = Type TypeRep
--           | List TypeRep
--           | ListOfAnything
--           | Set TypeRep
--           | SetOfAnything

checkType :: Key -> TypeRep -> Tx' (Maybe TypeError)
checkType key got = do
    result <- lookupType key
    case result of
        Nothing -> do
            return $ Just (Undeclared key)
        Just expected -> if expected == got
            then return $ Nothing
            else return $ Just (TypeMismatch key expected got)

    --
    -- ret <- returnStatus ["SET", key, en val]
    -- typeError <- checkType key (typeOf val)
    -- case typeError of
    --     Just err -> do
    --         assertError err
    --     Nothing -> return ret

    --
    -- val <- returnMaybe ["GET", key]
    -- typeError <- checkType key (carrier $ deferred val)
    -- case typeError of
    --     Just er -> do
    --         assertError er
    --     Nothing -> return val

checkType' :: Key -> Tx' a -> (a -> TypeRep) -> Tx' a
checkType' key cmd f = do
    returnValue <- cmd
    typeError <- checkType key (f returnValue)
    case typeError of
        Just er -> assertError er
        Nothing -> return returnValue



list :: TypeRep -> TypeRep
list = mkAppTy (typeRep (Proxy :: Proxy List))

carrier :: TypeRep -> TypeRep
carrier = head . typeRepArgs

deferred :: Typeable a => Deferred a -> TypeRep
deferred = carrier . typeOf
