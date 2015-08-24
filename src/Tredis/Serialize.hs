{-# LANGUAGE TypeFamilies, TypeOperators, DeriveDataTypeable #-}

module Tredis.Serialize where

import qualified Database.Redis as Redis
import           Database.Redis (Status(..))

import GHC.Generics
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Char8 (pack, unpack)
import Data.Typeable
import Data.Serialize
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import Data.Monoid (All, Any, Dual, Sum, Product, First, Last)
import Data.Ratio (Ratio)


class (Serialize a, Typeable a) => Value a where
    en :: a -> ByteString
    en = encode
    de :: ByteString -> Either String a
    de = decode

instance Value Int where
    en = pack . show
    de = Right . read . unpack

instance Value Integer where
    en = pack . show
    de = Right . read . unpack



instance Value Bool
instance Value Char
instance Value Double
instance Value Float
instance Value Int8
instance Value Int16
instance Value Int32
instance Value Int64
instance Value Ordering
instance Value Word
instance Value Word8
instance Value Word16
instance Value Word32
instance Value Word64
-- instance Value All
-- instance Value Any
instance Value ByteString
instance Value L.ByteString
instance Value a => Value [a]
instance (Value a, Integral a) => Value (Ratio a)
-- instance Value a => Value (Dual a)
-- instance Value a => Value (Sum a)
-- instance Value a => Value (Product a)
-- instance Value a => Value (First a)
-- instance Value a => Value (Last a)
instance Value a => Value (Maybe a)
instance (Value a, Value b) => Value (Either a b)
instance Value ()
instance (Value a, Value b) => Value (a, b)
instance (Value a, Value b, Value c) => Value (a, b, c)
instance (Value a, Value b, Value c, Value d) => Value (a, b, c, d)
instance (Value a, Value b, Value c, Value d, Value e) => Value (a, b, c, d, e)
instance (Value a, Value b, Value c, Value d, Value e, Value f) => Value (a, b, c, d, e, f)
instance (Value a, Value b, Value c, Value d, Value e, Value f, Value g) => Value (a, b, c, d, e, f, g)
-- instance (Value a, Value b, Value c, Value d, Value e, Value f, Value g, Value h) => Value (a, b, c, d, e, f, g, h)
-- instance (Value a, Value b, Value c, Value d, Value e, Value f, Value g, Value h, Value i) => Value (a, b, c, d, e, f, g, h, i)
-- instance (Value a, Value b, Value c, Value d, Value e, Value f, Value g, Value h, Value i, Value j) => Value (a, b, c, d, e, f, g, h, i, j)
