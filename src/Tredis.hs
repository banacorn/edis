module Tredis where

import Database.Redis as R hiding (decode)
import Data.Typeable
import Data.ByteString
import Data.Serialize as S
import Control.Monad.State
import Control.Applicative ((<$>))
import Data.Map as Map
import Data.Map

--------------------------------------------------------------------------------
--  encoding & decoding
--------------------------------------------------------------------------------

encodeValue :: Serialize a => a -> ByteString
encodeValue = S.encode


decodeValue :: Serialize a => Either Reply (Maybe ByteString) -> Either TredisReply (Maybe a)
decodeValue (Left err) = Left (RedisReply err)   -- error from Redis
decodeValue (Right Nothing) = Right Nothing      -- nil
decodeValue (Right (Just raw)) = case S.decode raw of
    Right val -> Right (Just val)           -- ok
    Left err  -> Left (DecodeError err)     -- decode error

--------------------------------------------------------------------------------
--  Tredis
--------------------------------------------------------------------------------

type Key = ByteString

-- Hash table for storing key-type relation
type TypeTable = Map Key TypeRep

type Tredis = StateT TypeTable Redis
data TredisReply = RedisReply Reply     -- original Redis error reply
                 | DecodeError String   -- decoding error
                 | Undeclared Key       -- unable to determine the type of a key
                 | TypeError Key TypeRep TypeRep
                 deriving (Show)

-- Redis command => Tredis command
liftCommand :: Redis (Either Reply a) -> Tredis (Either TredisReply a)
liftCommand f = lift f >>= return . mapLeft RedisReply
    where   mapLeft :: (a -> c) -> Either a b -> Either c b
            mapLeft f (Left a) = Left (f a)
            mapLeft _ (Right b) = Right b

liftAndDecode :: (Serialize a) => Redis (Either Reply (Maybe ByteString)) -> Tredis (Either TredisReply (Maybe a))
liftAndDecode f = lift f >>= return . decodeValue


runTredis :: Connection -> Tredis a -> IO a
runTredis conn f = runRedis conn (evalStateT f Map.empty)

--------------------------------------------------------------------------------
--  commands
--------------------------------------------------------------------------------

set :: (Serialize a, Typeable a) => ByteString -> a -> Tredis (Either TredisReply Status)
set key val = do
    modify $ insert key (typeOf val)
    liftCommand $ R.set key (encodeValue val)

get :: (Serialize a, Typeable a) => ByteString -> Tredis (Either TredisReply (Maybe a))
get key = liftAndDecode $ R.get key

incr :: ByteString -> Tredis (Either TredisReply Integer)
incr key = do
    result <- gets (Map.lookup key)
    case result of
        Just ty -> case ty == typeOfInteger of
            True -> liftCommand $ R.incr key                        -- Integer
            False -> return $ Left $ TypeError key typeOfInteger ty -- not Integer
        Nothing -> return $ Left $ Undeclared key       -- key-type relation not found
    where
            typeOfInteger :: TypeRep
            typeOfInteger = typeOf (0 :: Integer)
