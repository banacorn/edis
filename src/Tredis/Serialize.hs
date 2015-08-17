module Tredis.Serialize where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Char8 (pack, unpack)
import Data.Serialize (Serialize, encode, decode)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import Data.Monoid (All, Any, Dual, Sum, Product, First, Last)
import Data.Ratio (Ratio)

class Serialize a => Se a where
    en :: a -> ByteString
    en = encode
    de :: ByteString -> Either String a
    de = decode

instance Se Int where
    en = pack . show
    de = Right . read . unpack

instance Se Integer where
    en = pack . show
    de = Right . read . unpack

instance Se Bool
instance Se Char
instance Se Double
instance Se Float
instance Se Int8
instance Se Int16
instance Se Int32
instance Se Int64
instance Se Ordering
instance Se Word
instance Se Word8
instance Se Word16
instance Se Word32
instance Se Word64
instance Se All
instance Se Any
instance Se ByteString
instance Se L.ByteString
instance Se a => Se [a]
instance (Se a, Integral a) => Se (Ratio a)
instance Se a => Se (Dual a)
instance Se a => Se (Sum a)
instance Se a => Se (Product a)
instance Se a => Se (First a)
instance Se a => Se (Last a)
instance Se a => Se (Maybe a)
instance (Se a, Se b) => Se (Either a b)
instance Se ()
instance (Se a, Se b) => Se (a, b)
instance (Se a, Se b, Se c) => Se (a, b, c)
instance (Se a, Se b, Se c, Se d) => Se (a, b, c, d)
instance (Se a, Se b, Se c, Se d, Se e) => Se (a, b, c, d, e)
instance (Se a, Se b, Se c, Se d, Se e, Se f) => Se (a, b, c, d, e, f)
instance (Se a, Se b, Se c, Se d, Se e, Se f, Se g) => Se (a, b, c, d, e, f, g)
instance (Se a, Se b, Se c, Se d, Se e, Se f, Se g, Se h) => Se (a, b, c, d, e, f, g, h)
instance (Se a, Se b, Se c, Se d, Se e, Se f, Se g, Se h, Se i) => Se (a, b, c, d, e, f, g, h, i)
instance (Se a, Se b, Se c, Se d, Se e, Se f, Se g, Se h, Se i, Se j) => Se (a, b, c, d, e, f, g, h, i, j)
