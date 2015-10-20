module Edis.TypeChecking where

import           Edis.Transaction
import           Edis.Type
import           Data.Typeable

--------------------------------------------------------------------------------
--  type checking stuffs
--------------------------------------------------------------------------------

checkType :: KeySig -> Type -> Tx' (Maybe TypeError)
checkType keysig got = do
    result <- lookupType keysig
    case result of
        Nothing -> do
            return $ Just (Undeclared keysig)
        Just expected -> if expected == got
            then return $ Nothing
            else return $ Just (TypeMismatch keysig expected got)

compareResult :: Typeable a => KeySig -> Tx a -> (TypeRep -> Type) -> Tx a
compareResult keysig cmd f = do
    returnValue <- cmd
    typeErr <- checkType keysig (f $ carrier $ typeOf returnValue)
    case typeErr of
        Just er -> assertError er
        Nothing -> return returnValue

compareType :: Typeable a => KeySig -> Tx a -> Type -> Tx a
compareType keysig cmd t = compareResult keysig cmd (const t)

--------------------------------------------------------------------------------
--  TypeRep operators
--------------------------------------------------------------------------------

intTypeRep :: TypeRep
intTypeRep = typeRep (Proxy :: Proxy Int)

listTypeRep :: TypeRep
listTypeRep = typeRep (Proxy :: Proxy List)

setTypeRep :: TypeRep
setTypeRep = typeRep (Proxy :: Proxy Set)

hashTypeRep :: TypeRep
hashTypeRep = typeRep (Proxy :: Proxy Hash)

carrier :: TypeRep -> TypeRep
carrier = head . typeRepArgs
