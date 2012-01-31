{-# LANGUAGE BangPatterns, OverloadedStrings #-}

-- |
-- Module:      Data.Aeson.EncodeUtf8
-- Copyright:   (c) 2011 MailRank, Inc.
--              (c) 2012 Simon Meier <iridcode@gmail.com>
-- License:     Apache
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Efficiently serialize a JSON value using the UTF-8 encoding.

module Data.Aeson.EncodeUtf8
    (
      encode
    ) where

import Prelude hiding (null)
import Data.Aeson.Types (ToJSON(..), Value(..))
import Data.Attoparsec.Number (Number(..))
import Data.Monoid (mappend)
import Data.ByteString.Lazy.Builder               as B
import Data.ByteString.Lazy.Builder.ASCII         as B
import Data.ByteString.Lazy.Builder.BasicEncoding as E
-- import Data.Text.Lazy.Builder
-- import Data.Text.Lazy.Builder.Int (decimal)
-- import Data.Text.Lazy.Builder.RealFloat (realFloat)
-- import Data.Text.Lazy.Encoding (encodeUtf8)
-- import Numeric (showHex)
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import Data.Word
import Data.Char (ord)

-- | Encode a JSON value to a 'Builder'.  You can convert this to a
-- string using e.g. 'toLazyText', or encode straight to UTF-8 (the
-- standard JSON encoding) using 'encode'.
value :: Value -> Builder
value Null       = {-# SCC "fromValue/Null"   #-} null
value (Bool b)   = {-# SCC "fromValue/Bool"   #-} bool b
value (Number n) = {-# SCC "fromValue/Number" #-} number n
value (String s) = {-# SCC "fromValue/String" #-} string s
value (Array v)
    | V.null v = {-# SCC "value/Array" #-} B.char8 '[' <> B.char8 ']'
    | otherwise = {-# SCC "value/Array" #-}
                  B.char8 '[' <>
                  value (V.unsafeHead v) <>
                  V.foldr f (B.char8 ']') (V.unsafeTail v)
  where f a z = B.char8 ',' <> value a <> z
value (Object m) = {-# SCC "fromValue/Object" #-}
    case H.toList m of
      (x:xs) -> B.char8 '{' <> one x <> foldr f (B.char8 '}') xs
      _      -> B.char8 '{' <> B.char8 '}'
  where f a z     = B.char8 ',' <> one a <> z
        one (k,v) = string k <> B.char8 ':' <> value v

{-# INLINE null #-}
null :: Builder
null = E.encodeWithB (ascii4 ((('n','u'),'l'),'l')) ()

bool :: Bool -> Builder
bool = E.encodeWithB (ifB id (ascii4  ((('t','r'),'u'),'e'))
                             (ascii5 (((('f','a'),'l'),'s'),'e')))

string :: T.Text -> Builder
string s = {-# SCC "string" #-} 
    B.char8 '"' <> T.encodeUtf8Escaped escape s <> B.char8 '"'
  where
    c2w = fromIntegral . ord

    escape :: BoundedEncoding Word8
    escape =
      ifB (== c2w '\\'  ) (ascii2 ('\\','\\')) $
      ifB (== c2w '\"'  ) (ascii2 ('\\','"' )) $
      ifB (>= c2w '\x20') (fromF E.word8) $
      ifB (== c2w '\n'  ) (ascii2 ('\\','n' )) $
      ifB (== c2w '\r'  ) (ascii2 ('\\','r' )) $
      ifB (== c2w '\t'  ) (ascii2 ('\\','t' )) $
      (fromF hexChar) -- fallback for chars < 0x20

    hexChar :: FixedEncoding Word8
    hexChar = (\c -> (('\\','u'), fromIntegral c)) >$< 
      E.char8 `pairF` E.char8 `pairF` E.word16HexFixed

number :: Number -> Builder
number (I i) = B.integerDec i
number (D d)
    | isNaN d || isInfinite d = null
    | otherwise               = B.doubleDec d


-- | Efficiently serialize a JSON value as a lazy 'L.ByteString'.
encode :: ToJSON a => a -> L.ByteString
encode = {-# SCC "encode" #-} toLazyByteString . value .
         {-# SCC "toJSON" #-} toJSON
{-# INLINE encode #-}

(<>) :: Builder -> Builder -> Builder
(<>) = mappend
{-# INLINE (<>) #-}
infixr 6 <>

{-# INLINE ascii2 #-}
ascii2 :: (Char, Char) -> BoundedEncoding a
ascii2 cs = fromF $ (const cs) >$< E.char8 `pairF` E.char8

{-# INLINE ascii4 #-}
ascii4 :: (((Char, Char), Char), Char) -> BoundedEncoding a
ascii4 cs = fromF $ (const cs) >$< 
    E.char8 `pairF` E.char8 `pairF` E.char8 `pairF` E.char8

{-# INLINE ascii5 #-}
ascii5 :: ((((Char, Char), Char), Char), Char) -> BoundedEncoding a
ascii5 cs = fromF $ (const cs) >$< 
    E.char8 `pairF` E.char8 `pairF` E.char8 `pairF` E.char8 `pairF` E.char8

