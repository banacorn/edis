module Tredis where

import Database.Redis as R hiding (decode, put)
import Data.Typeable
import Data.ByteString
import Data.Serialize as S hiding (get, put)
import Control.Monad.State as State
import Control.Applicative ((<$>))
import Data.Map as Map
import Data.Map

--------------------------------------------------------------------------------
--  encoding & decoding
--------------------------------------------------------------------------------

encodeValue :: Serialize a => a -> ByteString
encodeValue = S.encode

decodeValue :: Serialize a => Maybe ByteString -> Tredis (TredisReply (Maybe a))
decodeValue Nothing = return (Right Nothing)
decodeValue (Just raw) = case S.decode raw of
    Right val -> return $ Right (Just val)
    Left err -> do
        assertError $ DecodeError err
        return $ Left (DecodeError err)

-- decodeValue Nothing    = Nothing
-- decodeValue (Just raw) = case S.decode raw of
--     Right val -> Just val            -- ok
--     Left err  -> Nothing             -- decode error

-- decodeValue (Left err) = Left (RedisReply err)   -- error from Redis
-- decodeValue (Right Nothing) = Right Nothing      -- nil
-- decodeValue (Right (Just raw)) = case S.decode raw of
--     Right val -> Right (Just val)           -- ok
--     Left err  -> Left (DecodeError err)     -- decode error

--------------------------------------------------------------------------------
--  Tredis
--------------------------------------------------------------------------------

type TredisReply a = Either TredisError a

type Key = ByteString
-- Hash table for storing key-type relation
type TypeTable = Map Key TypeRep
data TredisState = TredisState
    {   typeTable :: TypeTable
    ,   typeError :: [TredisError]
    }

defaultTredisState :: TredisState
defaultTredisState = TredisState Map.empty []

type Tredis = StateT TredisState Redis
data TredisError = RedisReply Reply     -- original Redis error reply
                 | DecodeError String   -- decoding error
                 | Undeclared Key       -- unable to determine the type of a key
                 | TypeMismatch Key TypeRep TypeRep
                 deriving (Show)

-- assert key-type relation
assertType :: Key -> TypeRep -> Tredis ()
assertType key typ = do
    state <- State.get
    let table = typeTable state
    State.put (state { typeTable = Map.insert key typ table })

lookupType :: Key -> Tredis (Maybe TypeRep)
lookupType key = do
    state <- State.get
    let table = typeTable state
    return $ Map.lookup key table

assertError :: TredisError -> Tredis ()
assertError e = do
    state <- State.get
    let errors = typeError state
    State.put (state { typeError = e : errors })

liftCommand :: Redis (Either Reply a) -> Tredis (TredisReply a)
liftCommand f = lift f >>= return . mapLeft RedisReply
    where   mapLeft :: (a -> c) -> Either a b -> Either c b
            mapLeft f (Left a) = Left (f a)
            mapLeft f (Right b) = Right b

liftDecode :: (Serialize a) => Redis (Either Reply (Maybe ByteString)) -> Tredis (TredisReply (Maybe a))
liftDecode f = lift f >>= g
    where   g (Left a)        = return $ Left (RedisReply a)
            g (Right raw)     = decodeValue raw

runTredis :: Connection -> Tredis a -> IO (Either [TredisError] a)
runTredis conn f = runRedis conn $ do
    (value, state) <- runStateT f defaultTredisState
    let typeErrors = typeError state
    if Prelude.null typeErrors
        then return (Right value)
        else return (Left typeErrors)

--------------------------------------------------------------------------------
--  commands
--------------------------------------------------------------------------------

set :: (Serialize a, Typeable a) => ByteString -> a -> Tredis (TredisReply Status)
set key val = do
    assertType key (typeOf val)
    liftCommand $ R.set key (encodeValue val)

get :: (Serialize a, Typeable a) => ByteString -> Tredis (TredisReply (Maybe a))
get key = liftDecode $ R.get key

incr :: ByteString -> Tredis (TredisReply Integer)
incr key = do
    result <- lookupType key
    case result of
        Just ty -> case ty == typeOfInteger of
            True -> liftCommand $ R.incr key
            False -> do
                assertError (TypeMismatch key typeOfInteger ty)
                return $ Left (TypeMismatch key typeOfInteger ty)
        Nothing -> do
            assertError (Undeclared key)
            return $ Left (Undeclared key)
    where
            typeOfInteger :: TypeRep
            typeOfInteger = typeOf (0 :: Integer)
