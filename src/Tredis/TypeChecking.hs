module Tredis.TypeChecking where

import           Tredis.Transaction
import           Tredis.Type
import           Data.Typeable

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

compareResult :: Typeable a => Key -> Tx a -> (TypeRep -> Type) -> Tx a
compareResult key cmd f = do
    returnValue <- cmd
    typeErr <- checkType key (f $ carrier $ typeOf returnValue)
    case typeErr of
        Just er -> assertError er
        Nothing -> return returnValue

compareType :: Typeable a => Key -> Tx a -> Type -> Tx a
compareType key cmd t = compareResult key cmd (const t)

--------------------------------------------------------------------------------
--  TypeRep operators
--------------------------------------------------------------------------------

intTypeRep :: TypeRep
intTypeRep = typeRep (Proxy :: Proxy Int)

listTypeRep :: TypeRep
listTypeRep = typeRep (Proxy :: Proxy List)

setTypeRep :: TypeRep
setTypeRep = typeRep (Proxy :: Proxy Set)

carrier :: TypeRep -> TypeRep
carrier = head . typeRepArgs
