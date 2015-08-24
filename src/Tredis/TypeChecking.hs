module Tredis.TypeChecking where

import           Tredis.Transaction
import           Tredis.Type
import           Data.Typeable
import           GHC.Generics

--------------------------------------------------------------------------------
--  type checking stuffs
--------------------------------------------------------------------------------

checkType :: Key -> Type -> Tx' (Maybe TypeError)
checkType key got = do
    result <- lookupType key
    case result of
        Nothing -> do
            return $ Just (Undeclared key)
        Just expected -> if expected == got
            then return $ Nothing
            else return $ Just (TypeMismatch key expected got)

compareCmdType :: Key -> Tx' a -> (a -> Type) -> Tx' a
compareCmdType key cmd f = do
    returnValue <- cmd
    typeError <- checkType key (f returnValue)
    case typeError of
        Just er -> assertError er
        Nothing -> return returnValue

compareType :: Key -> Tx' a -> Type -> Tx' a
compareType key cmd typ = compareCmdType key cmd (const typ)



intTypeRep :: TypeRep
intTypeRep = typeRep (Proxy :: Proxy Int)

listTypeRep :: TypeRep
listTypeRep = typeRep (Proxy :: Proxy RList)

setTypeRep :: TypeRep
setTypeRep = typeRep (Proxy :: Proxy RSet)

carrier :: TypeRep -> TypeRep
carrier = head . typeRepArgs

deferred :: Typeable a => Deferred a -> TypeRep
deferred = carrier . typeOf

toListType  :: Typeable n => RList n -> Type
toListType  (RList n) = ListType  (typeOf n)
