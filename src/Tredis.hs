module Tredis where

import Database.Redis as R hiding (decode)
import Data.ByteString
import Data.Serialize as S
import Control.Monad.Trans (liftIO)

data E = ConnE Reply | SemE | DecE String deriving (Show)

encode :: Serialize a => a -> ByteString
encode = S.encode

decode :: Serialize a => Either Reply (Maybe ByteString) -> Redis (Either E a)
decode (Right reply) = return $ case reply of
    Just raw -> case S.decode raw of
        Right val -> Right val
        Left err  -> Left (DecE err)
    Nothing  -> Left SemE
decode (Left err) = return $ Left (ConnE err)
