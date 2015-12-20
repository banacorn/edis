module Database.Edis.Promoted.Helper where

import Data.ByteString          (ByteString)
import Data.Maybe               (fromJust)
import GHC.TypeLits
import Data.Proxy               (Proxy)
import Data.Serialize           (Serialize, encode, decode)
import Database.Redis as Redis hiding (decode)

--------------------------------------------------------------------------------
--  Helper functions
--------------------------------------------------------------------------------

encodeKey :: KnownSymbol s => Proxy s -> ByteString
encodeKey = encode . symbolVal

decodeAsAnything :: (Serialize x) => Either Reply ByteString -> Redis (Either Reply x)
decodeAsAnything (Left replyErr) = return $ Left replyErr
decodeAsAnything (Right str) = case decode str of
        Left decodeErr -> return $ Left (Error $ encode decodeErr)
        Right val      -> return $ Right val

decodeAsMaybe :: (Serialize x) => Either Reply (Maybe ByteString) -> Redis (Either Reply (Maybe x))
decodeAsMaybe (Left replyErr) = return $ Left replyErr
decodeAsMaybe (Right Nothing) = return $ Right Nothing
decodeAsMaybe (Right (Just str)) = case decode str of
        Left decodeErr -> return $ Left (Error $ encode decodeErr)
        Right val      -> return $ Right (Just val)

decodeBPOP :: Serialize x => Either Reply (Maybe (ByteString, ByteString)) -> Redis (Either Reply (Maybe (ByteString, x)))
decodeBPOP (Left replyError)         = return $ Left replyError
decodeBPOP (Right Nothing)           = return $ Right Nothing
decodeBPOP (Right (Just (key, raw))) = case decode raw of
    Left decodeError -> return $ Left (Error $ encode decodeError)
    Right value      -> return $ Right (Just (key, value))

decodeAsList :: (Serialize x) => Either Reply [ByteString] -> Redis (Either Reply [x])
decodeAsList (Left replyErr) = return $ Left replyErr
decodeAsList (Right strs) = case mapM decode strs of
    Left decodeErr -> return $ Left (Error $ encode decodeErr)
    Right vals     -> return $ Right vals

decodeAsListWithScores :: (Serialize x) => Either Reply [(ByteString, Double)] -> Redis (Either Reply [(x, Double)])
decodeAsListWithScores (Left replyErr) = return $ Left replyErr
decodeAsListWithScores (Right pairs) = let (strs, scores) = unzip pairs in
    case mapM decode strs of
        Left decodeErr -> return $ Left (Error $ encode decodeErr)
        Right vals     -> return $ Right (zip vals scores)

fromRight :: Either a b -> b
fromRight (Left _) = error "Left val"
fromRight (Right e) = e

fromMaybeResult :: Either Reply (Maybe x) -> x
fromMaybeResult = fromJust . fromRight
