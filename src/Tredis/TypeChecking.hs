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

compareCmdType :: Key -> Tx' a -> (a -> TypeRep) -> Tx' a
compareCmdType key cmd f = do
    returnValue <- cmd
    typeError <- checkType key (f returnValue)
    case typeError of
        Just er -> assertError er
        Nothing -> return returnValue

compareType :: Key -> Tx' a -> TypeRep -> Tx' a
compareType key cmd typ = compareCmdType key cmd (const typ)


list :: TypeRep -> TypeRep
list = mkAppTy (typeRep (Proxy :: Proxy List))

carrier :: TypeRep -> TypeRep
carrier = head . typeRepArgs

deferred :: Typeable a => Deferred a -> TypeRep
deferred = carrier . typeOf
