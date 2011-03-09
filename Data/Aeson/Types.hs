{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, GeneralizedNewtypeDeriving,
    IncoherentInstances, OverlappingInstances, Rank2Types #-}

-- Module:      Data.Aeson.Types
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Bryan O'Sullivan <bos@mailrank.com>
-- Stability:   experimental
-- Portability: portable
--
-- Types for working with JSON data.

module Data.Aeson.Types
    (
    -- * Core JSON types
      Value(..)
    , Array
    , emptyArray
    , Pair
    , Object
    , emptyObject
    -- * Convenience types
    , DotNetTime(..)
    -- * Type conversion
    , Parser
    , Result(..)
    , FromJSON(..)
    , fromJSON
    , parse
    , ToJSON(..)
    -- * Constructors and accessors
    , (.=)
    , (.:)
    , (.:?)
    , object
    ) where

import Control.Applicative
import Control.DeepSeq (NFData(..))
import Control.Monad (MonadPlus(..))
import Data.Aeson.Functions
import Data.Attoparsec.Char8 (Number(..))
import Data.Data (Data)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Map (Map)
import Data.Monoid (Dual(..), First(..), Last(..))
import Data.Monoid (Monoid(..))
import Data.Ratio (Ratio)
import Data.String (IsString(..))
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (FormatTime, formatTime, parseTime)
import Data.Typeable (Typeable)
import Data.Vector (Vector)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import System.Locale (defaultTimeLocale)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.HashMap.Strict as H
import qualified Data.IntSet as IntSet
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Vector as V

-- | The result of running a 'Parser'.
data Result a = Error String
              | Success a
                deriving (Eq, Show, Typeable)

-- | Failure continuation.  Constructs an 'Error'.
type Failure r   = String -> Result r
-- | Success continuation.  Constructs a 'Success'.
type Success a r = a -> Result r

-- | A continuation-based parser type.
newtype Parser a = Parser {
      runParser :: forall r.
                   Failure r
                -> Success a r
                -> Result r
    }

instance Monad Parser where
    m >>= g = Parser $ \kf ks -> let ks' a = runParser (g a) kf ks
                                 in runParser m kf ks'
    {-# INLINE (>>=) #-}
    return a = Parser $ \_kf ks -> ks a
    {-# INLINE return #-}
    fail msg = Parser $ \kf _ks -> kf msg
    {-# INLINE fail #-}

instance Functor Parser where
    fmap f m = Parser $ \kf ks -> let ks' a = ks (f a)
                                  in runParser m kf ks'
    {-# INLINE fmap #-}

instance Applicative Parser where
    pure  = return
    {-# INLINE pure #-}
    (<*>) = apP
    {-# INLINE (<*>) #-}
    
instance Alternative Parser where
    empty = fail "empty"
    {-# INLINE empty #-}
    (<|>) = mplus
    {-# INLINE (<|>) #-}

instance MonadPlus Parser where
    mzero = fail "mzero"
    {-# INLINE mzero #-}
    mplus a b = Parser $ \kf ks -> let kf' _ = runParser b kf ks
                                   in runParser a kf' ks
    {-# INLINE mplus #-}

instance Monoid (Parser a) where
    mempty  = fail "mempty"
    {-# INLINE mempty #-}
    mappend = mplus
    {-# INLINE mappend #-}

apP :: Parser (a -> b) -> Parser a -> Parser b
apP d e = do
  b <- d
  a <- e
  return (b a)
{-# INLINE apP #-}

-- | A JSON \"object\" (key\/value map).
type Object = Map Text Value

-- | A JSON \"array\" (sequence).
type Array = Vector Value

-- | A JSON value represented as a Haskell value.
data Value = Object Object
           | Array Array
           | String Text
           | Number Number
           | Bool !Bool
           | Null
             deriving (Eq, Show, Typeable, Data)

instance NFData Value where
    rnf (Object o) = rnf o
    rnf (Array a)  = V.foldl' (\x y -> rnf y `seq` x) () a
    rnf (String s) = rnf s
    rnf (Number n) = case n of I i -> rnf i; D d -> rnf d
    rnf (Bool b)   = rnf b
    rnf Null       = ()

instance IsString Value where
    fromString = String . pack
    {-# INLINE fromString #-}

-- | The empty array.
emptyArray :: Value
emptyArray = Array V.empty

-- | The empty object.
emptyObject :: Value
emptyObject = Object M.empty

-- | A key\/value pair for an 'Object'.
type Pair = (Text, Value)

-- | Construct a 'Pair' from a key and a value.
(.=) :: ToJSON a => Text -> a -> Pair
name .= value = (name, toJSON value)
{-# INLINE (.=) #-}

-- | Convert a value from JSON, failing if the types do not match.
fromJSON :: (FromJSON a) => Value -> Result a
fromJSON = parse parseJSON
{-# INLINE fromJSON #-}

-- | Run a 'Parser'.
parse :: (a -> Parser b) -> a -> Result b
parse m v = runParser (m v) Error Success
{-# INLINE parse #-}

-- | Retrieve the value associated with the given key of an 'Object'.
-- The result is 'empty' if the key is not present or the value cannot
-- be converted to the desired type.
--
-- This accessor is appropriate if the key and value /must/ be present
-- in an object for it to be valid.  If the key and value are
-- optional, use '(.:?)' instead.
(.:) :: (FromJSON a) => Object -> Text -> Parser a
obj .: key = case M.lookup key obj of
               Nothing -> empty
               Just v  -> parseJSON v
{-# INLINE (.:) #-}

-- | Retrieve the value associated with the given key of an 'Object'.
-- The result is 'Nothing' if the key is not present, or 'empty' if
-- the value cannot be converted to the desired type.
--
-- This accessor is most useful if the key and value can be absent
-- from an object without affecting its validity.  If the key and
-- value are mandatory, use '(.:)' instead.
(.:?) :: (FromJSON a) => Object -> Text -> Parser (Maybe a)
obj .:? key = case M.lookup key obj of
               Nothing -> pure Nothing
               Just v  -> parseJSON v
{-# INLINE (.:?) #-}

-- | Create a 'Value' from a list of name\/value 'Pair's.  If duplicate
-- keys arise, earlier keys and their associated values win.
object :: [Pair] -> Value
object = Object . M.fromList
{-# INLINE object #-}

-- | A type that can be converted to JSON.
--
-- An example type and instance:
--
-- @data Coord { x :: Double, y :: Double }
--
-- instance ToJSON Coord where
--   toJSON (Coord x y) = 'object' [\"x\" '.=' x, \"y\" '.=' y]
-- @
class ToJSON a where
    toJSON   :: a -> Value

-- | A type that can be converted from JSON, with the possibility of
-- failure.
--
-- When writing an instance, use 'mzero' or 'fail' to make a
-- conversion fail, e.g. if an 'Object' is missing a required key, or
-- the value is of the wrong type.
--
-- An example type and instance:
--
-- @data Coord { x :: Double, y :: Double }
-- 
-- instance FromJSON Coord where
--   parseJSON ('Object' v) = Coord '<$>'
--                         v '.:' \"x\" '<*>'
--                         v '.:' \"y\"
--
--   \-- A non-'Object' value is of the wrong type, so use 'mzero' to fail.
--   parseJSON _          = 'mzero'
-- @
class FromJSON a where
    parseJSON :: Value -> Parser a

instance (ToJSON a) => ToJSON (Maybe a) where
    toJSON (Just a) = toJSON a
    toJSON Nothing  = Null
    {-# INLINE toJSON #-}
    
instance (FromJSON a) => FromJSON (Maybe a) where
    parseJSON Null   = pure Nothing
    parseJSON a      = Just <$> parseJSON a
    {-# INLINE parseJSON #-}

instance (ToJSON a, ToJSON b) => ToJSON (Either a b) where
    toJSON (Left a)  = toJSON a
    toJSON (Right b) = toJSON b
    {-# INLINE toJSON #-}
    
instance (FromJSON a, FromJSON b) => FromJSON (Either a b) where
    parseJSON a = Left <$> parseJSON a <|> Right <$> parseJSON a
    {-# INLINE parseJSON #-}

instance ToJSON Bool where
    toJSON = Bool
    {-# INLINE toJSON #-}

instance FromJSON Bool where
    parseJSON (Bool b) = pure b
    parseJSON _        = empty
    {-# INLINE parseJSON #-}

instance ToJSON () where
    toJSON _ = emptyArray
    {-# INLINE toJSON #-}

instance FromJSON () where
    parseJSON (Array v) | V.null v = pure ()
    parseJSON _                    = empty
    {-# INLINE parseJSON #-}

instance ToJSON [Char] where
    toJSON = String . T.pack
    {-# INLINE toJSON #-}

instance FromJSON [Char] where
    parseJSON (String t) = pure (T.unpack t)
    parseJSON _          = empty
    {-# INLINE parseJSON #-}

instance ToJSON Char where
    toJSON = String . T.singleton
    {-# INLINE toJSON #-}

instance FromJSON Char where
    parseJSON (String t)
        | T.compareLength t 1 == EQ = pure (T.head t)
    parseJSON _                      = empty
    {-# INLINE parseJSON #-}

instance ToJSON Double where
    toJSON = Number . D
    {-# INLINE toJSON #-}

instance FromJSON Double where
    parseJSON (Number n) = case n of
                             D d -> pure d
                             I i -> pure (fromIntegral i)
    parseJSON _              = empty
    {-# INLINE parseJSON #-}

instance ToJSON Number where
    toJSON = Number
    {-# INLINE toJSON #-}

instance FromJSON Number where
    parseJSON (Number n) = pure n
    parseJSON _          = empty
    {-# INLINE parseJSON #-}

instance ToJSON Float where
    toJSON = Number . fromRational . toRational
    {-# INLINE toJSON #-}

instance FromJSON Float where
    parseJSON (Number n) = case n of
                             D d -> pure . fromRational . toRational $ d
                             I i -> pure (fromIntegral i)
    parseJSON _          = empty
    {-# INLINE parseJSON #-}

instance ToJSON (Ratio Integer) where
    toJSON = Number . fromRational
    {-# INLINE toJSON #-}

instance FromJSON (Ratio Integer) where
    parseJSON (Number n) = case n of
                             D d -> pure . toRational $ d
                             I i -> pure (fromIntegral i)
    parseJSON _          = empty
    {-# INLINE parseJSON #-}

instance ToJSON Int where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

instance FromJSON Int where
    parseJSON = parseIntegral
    {-# INLINE parseJSON #-}

parseIntegral :: Integral a => Value -> Parser a
parseIntegral (Number n) = pure (floor n)
parseIntegral _          = empty
{-# INLINE parseIntegral #-}

instance ToJSON Integer where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

instance FromJSON Integer where
    parseJSON = parseIntegral
    {-# INLINE parseJSON #-}

instance ToJSON Int8 where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

instance FromJSON Int8 where
    parseJSON = parseIntegral
    {-# INLINE parseJSON #-}

instance ToJSON Int16 where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

instance FromJSON Int16 where
    parseJSON = parseIntegral
    {-# INLINE parseJSON #-}

instance ToJSON Int32 where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

instance FromJSON Int32 where
    parseJSON = parseIntegral
    {-# INLINE parseJSON #-}

instance ToJSON Int64 where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

instance FromJSON Int64 where
    parseJSON = parseIntegral
    {-# INLINE parseJSON #-}

instance ToJSON Word where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

instance FromJSON Word where
    parseJSON = parseIntegral
    {-# INLINE parseJSON #-}

instance ToJSON Word8 where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

instance FromJSON Word8 where
    parseJSON = parseIntegral
    {-# INLINE parseJSON #-}

instance ToJSON Word16 where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

instance FromJSON Word16 where
    parseJSON = parseIntegral
    {-# INLINE parseJSON #-}

instance ToJSON Word32 where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

instance FromJSON Word32 where
    parseJSON = parseIntegral
    {-# INLINE parseJSON #-}

instance ToJSON Word64 where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

instance FromJSON Word64 where
    parseJSON = parseIntegral
    {-# INLINE parseJSON #-}

instance ToJSON Text where
    toJSON = String
    {-# INLINE toJSON #-}

instance FromJSON Text where
    parseJSON (String t) = pure t
    parseJSON _          = empty
    {-# INLINE parseJSON #-}

instance ToJSON LT.Text where
    toJSON = String . LT.toStrict
    {-# INLINE toJSON #-}

instance FromJSON LT.Text where
    parseJSON (String t) = pure (LT.fromStrict t)
    parseJSON _          = empty
    {-# INLINE parseJSON #-}

instance ToJSON B.ByteString where
    toJSON = String . decode
    {-# INLINE toJSON #-}

instance FromJSON B.ByteString where
    parseJSON (String t) = pure . encodeUtf8 $ t
    parseJSON _          = empty
    {-# INLINE parseJSON #-}

instance ToJSON LB.ByteString where
    toJSON = toJSON . strict
    {-# INLINE toJSON #-}

instance FromJSON LB.ByteString where
    parseJSON (String t) = pure . lazy $ t
    parseJSON _          = empty
    {-# INLINE parseJSON #-}

instance (ToJSON a) => ToJSON [a] where
    toJSON = Array . V.fromList . map toJSON
    {-# INLINE toJSON #-}
    
instance (FromJSON a) => FromJSON [a] where
    parseJSON (Array a) = mapM parseJSON (V.toList a)
    parseJSON _         = empty
    {-# INLINE parseJSON #-}

instance (ToJSON a) => ToJSON (Vector a) where
    toJSON = Array . V.map toJSON
    {-# INLINE toJSON #-}
    
instance (FromJSON a) => FromJSON (Vector a) where
    parseJSON (Array a) = V.mapM parseJSON a
    parseJSON _         = empty
    {-# INLINE parseJSON #-}

instance (ToJSON a) => ToJSON (Set.Set a) where
    toJSON = toJSON . Set.toList
    {-# INLINE toJSON #-}
    
instance (Ord a, FromJSON a) => FromJSON (Set.Set a) where
    parseJSON = fmap Set.fromList . parseJSON
    {-# INLINE parseJSON #-}

instance ToJSON IntSet.IntSet where
    toJSON = toJSON . IntSet.toList
    {-# INLINE toJSON #-}
    
instance FromJSON IntSet.IntSet where
    parseJSON = fmap IntSet.fromList . parseJSON
    {-# INLINE parseJSON #-}

instance (ToJSON v) => ToJSON (M.Map Text v) where
    toJSON = Object . M.map toJSON
    {-# INLINE toJSON #-}

instance (FromJSON v) => FromJSON (M.Map Text v) where
    parseJSON (Object o) = M.fromAscList <$> mapM go (M.toAscList o)
      where go (k,v)     = ((,) k) <$> parseJSON v
    parseJSON _          = empty

instance (ToJSON v) => ToJSON (M.Map LT.Text v) where
    toJSON = Object . transformMap LT.toStrict toJSON

instance (FromJSON v) => FromJSON (M.Map LT.Text v) where
    parseJSON = fmap (M.mapKeysMonotonic LT.fromStrict) . parseJSON

instance (ToJSON v) => ToJSON (M.Map String v) where
    toJSON = Object . transformMap pack toJSON

instance (FromJSON v) => FromJSON (M.Map String v) where
    parseJSON = fmap (M.mapKeysMonotonic unpack) . parseJSON

instance (ToJSON v) => ToJSON (M.Map B.ByteString v) where
    toJSON = Object . transformMap decode toJSON

instance (FromJSON v) => FromJSON (M.Map B.ByteString v) where
    parseJSON = fmap (M.mapKeysMonotonic encodeUtf8) . parseJSON

instance (ToJSON v) => ToJSON (M.Map LB.ByteString v) where
    toJSON = Object . transformMap strict toJSON

instance (FromJSON v) => FromJSON (M.Map LB.ByteString v) where
    parseJSON = fmap (M.mapKeysMonotonic lazy) . parseJSON

instance (ToJSON v) => ToJSON (H.HashMap Text v) where
    toJSON = Object . hashMap id toJSON
    {-# INLINE toJSON #-}

instance (FromJSON v) => FromJSON (H.HashMap Text v) where
    parseJSON (Object o) = H.fromList <$> mapM go (M.toList o)
      where go (k,v)     = ((,) k) <$> parseJSON v
    parseJSON _          = empty

instance (ToJSON v) => ToJSON (H.HashMap LT.Text v) where
    toJSON = Object . M.fromList . H.foldrWithKey (\k v -> ((LT.toStrict k,toJSON v) :)) []

instance (FromJSON v) => FromJSON (H.HashMap LT.Text v) where
    parseJSON = fmap (mapHash LT.fromStrict) . parseJSON

instance (ToJSON v) => ToJSON (H.HashMap String v) where
    toJSON = Object . hashMap pack toJSON

instance (FromJSON v) => FromJSON (H.HashMap String v) where
    parseJSON = fmap (mapHash unpack) . parseJSON

instance (ToJSON v) => ToJSON (H.HashMap B.ByteString v) where
    toJSON = Object . hashMap decode toJSON

instance (FromJSON v) => FromJSON (H.HashMap B.ByteString v) where
    parseJSON = fmap (mapHash encodeUtf8) . parseJSON

instance (ToJSON v) => ToJSON (H.HashMap LB.ByteString v) where
    toJSON = Object . hashMap strict toJSON

instance (FromJSON v) => FromJSON (H.HashMap LB.ByteString v) where
    parseJSON = fmap (mapHash lazy) . parseJSON

instance ToJSON Value where
    toJSON a = a
    {-# INLINE toJSON #-}

instance FromJSON Value where
    parseJSON a = pure a
    {-# INLINE parseJSON #-}

-- | A newtype wrapper for 'UTCTime' that uses the same non-standard
-- serialization format as Microsoft .NET.
newtype DotNetTime = DotNetTime UTCTime
    deriving (Eq, Ord, Read, Show, Typeable, FormatTime)

instance ToJSON DotNetTime where
    toJSON (DotNetTime t) =
        String (pack (formatTime defaultTimeLocale "/Date(%s)/" t))
    {-# INLINE toJSON #-}

instance FromJSON DotNetTime where
    parseJSON (String t) =
        case parseTime defaultTimeLocale "/Date(%s)/" (unpack t) of
          Just d -> pure (DotNetTime d)
          _      -> empty
    parseJSON _          = empty
    {-# INLINE parseJSON #-}

instance ToJSON UTCTime where
    toJSON t = String (pack (formatTime defaultTimeLocale "%FT%X%QZ" t))
    {-# INLINE toJSON #-}

instance FromJSON UTCTime where
    parseJSON (String t) =
        case parseTime defaultTimeLocale "%FT%X%QZ" (unpack t) of
          Just d -> pure d
          _      -> empty
    parseJSON _          = empty
    {-# INLINE parseJSON #-}

instance (ToJSON a, ToJSON b) => ToJSON (a,b) where
    toJSON (a,b) = toJSON [toJSON a, toJSON b]
    {-# INLINE toJSON #-}

instance (FromJSON a, FromJSON b) => FromJSON (a,b) where
    parseJSON (Array ab) = case V.toList ab of
                            [a,b] -> (,) <$> parseJSON a <*> parseJSON b
                            _     -> empty
    parseJSON _          = empty
    {-# INLINE parseJSON #-}

instance ToJSON a => ToJSON (Dual a) where
    toJSON = toJSON . getDual
    {-# INLINE toJSON #-}

instance FromJSON a => FromJSON (Dual a) where
    parseJSON = fmap Dual . parseJSON
    {-# INLINE parseJSON #-}

instance ToJSON a => ToJSON (First a) where
    toJSON = toJSON . getFirst
    {-# INLINE toJSON #-}

instance FromJSON a => FromJSON (First a) where
    parseJSON = fmap First . parseJSON
    {-# INLINE parseJSON #-}

instance ToJSON a => ToJSON (Last a) where
    toJSON = toJSON . getLast
    {-# INLINE toJSON #-}

instance FromJSON a => FromJSON (Last a) where
    parseJSON = fmap Last . parseJSON
    {-# INLINE parseJSON #-}
