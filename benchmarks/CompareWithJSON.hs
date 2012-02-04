{-# OPTIONS_GHC -fno-warn-orphans #-}

import Control.DeepSeq (NFData(rnf))
import Criterion.Main
import qualified Data.Aeson as A
import qualified Data.Aeson.EncodeUtf8 as AUTF8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Builder  as B
import qualified Text.JSON as J
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy.Builder.BasicEncoding as E

instance (NFData v) => NFData (J.JSObject v) where
  rnf o = rnf (J.fromJSObject o)

instance NFData J.JSValue where
  rnf J.JSNull = ()
  rnf (J.JSBool b) = rnf b
  rnf (J.JSRational a b) = rnf a `seq` rnf b `seq` ()
  rnf (J.JSString s) = rnf (J.fromJSString s)
  rnf (J.JSArray lst) = rnf lst
  rnf (J.JSObject o) = rnf o

decodeJ :: String -> J.JSValue
decodeJ s =
  case J.decodeStrict s of
    J.Ok v -> v
    J.Error _ -> error "fail to parse via JSON"

decodeA :: BL.ByteString -> A.Value
decodeA s = case A.decode s of
              Just v -> v
              Nothing -> error "fail to parse via Aeson"

encodeJ :: J.JSValue -> BL.ByteString
encodeJ = B.toLazyByteString . B.stringUtf8 . J.encode

builderEncodeUtf8 :: T.Text -> BL.ByteString
builderEncodeUtf8 = B.toLazyByteString . T.encodeTextWithB E.charUtf8

builderEncodeUtf8New :: T.Text -> BL.ByteString
builderEncodeUtf8New = B.toLazyByteString . T.encodeUtf8Builder

main :: IO ()
main = do
  let enFile = "json-data/twitter100.json"
      jpFile = "json-data/jp100.json"
      numFile = "json-data/numbers.json"
      intFile = "json-data/integers.json"
  enA <- BL.readFile enFile
  enJ <- readFile enFile
  jpA <- BL.readFile jpFile
  jpJ <- readFile jpFile
  numA <- BL.readFile numFile
  numJ <- readFile numFile
  intA <- BL.readFile intFile
  intJ <- readFile intFile
  let enText = T.pack enJ
      jpText = T.pack enJ
      
      equal [] = True
      equal xs = and $ zipWith (==) xs (tail xs)

  if (all equal [ [ A.encode (decodeA enA) 
                  , AUTF8.encode (decodeA enA)
                  -- , encodeJ (decodeJ enJ)
                  ]
                , [ A.encode (decodeA jpA)
                  , AUTF8.encode (decodeA jpA)
                  -- , encodeJ (decodeJ jpJ)
                  ]
                , [ A.encode (decodeA numA)
                  , AUTF8.encode (decodeA numA)
                  -- , encodeJ (decodeJ numJ)
                  ]
                , [ A.encode (decodeA intA)
                  , AUTF8.encode (decodeA intA)
                  -- , encodeJ (decodeJ intJ)
                  ]
                ] )
    then putStrLn $ "GOOD: all aeson encoders yield the same result."
    else putStrLn $ "WARNING: some aeson encoders yield different result."
  defaultMain [
      bgroup "decode" [
        bgroup "en" [
          bench "aeson" $ nf decodeA enA
        , bench "json"  $ nf decodeJ enJ
        ]
      , bgroup "jp" [
          bench "aeson" $ nf decodeA jpA
        , bench "json"  $ nf decodeJ jpJ
        ]
      ]
    , bgroup "utf8-encoding" [
        bgroup "en" [
          bench "text-hand-coded"     $ nf T.encodeUtf8      enText
        , bench "builder-new"  $ nf builderEncodeUtf8New enText
        , bench "builder-streaming"  $ nf builderEncodeUtf8 enText
        ]
      , bgroup "jp" [
          bench "text-hand-coded"    $ nf T.encodeUtf8      jpText
        , bench "builder-new" $ nf builderEncodeUtf8New jpText
        , bench "builder-streaming" $ nf builderEncodeUtf8 jpText
        ]
      ]
    , bgroup "encode" [
        bgroup "en" [
          bench "aeson-text-builder" $ nf A.encode (decodeA enA)
        , bench "aeson-new-builder" $ nf AUTF8.encode (decodeA enA)
        , bench "json"  $ nf encodeJ (decodeJ enJ)
        ]
      , bgroup "jp" [
          bench "aeson-text-builder" $ nf A.encode (decodeA jpA)
        , bench "aeson-new-builder" $ nf AUTF8.encode (decodeA jpA)
        , bench "json"  $ nf encodeJ (decodeJ jpJ)
        ]
      , bgroup "num" [
          bench "aeson-text-builder" $ nf A.encode (decodeA numA)
        , bench "aeson-new-builder" $ nf AUTF8.encode (decodeA numA)
        , bench "json"  $ nf encodeJ (decodeJ numJ)
        ]
      , bgroup "int" [
          bench "aeson" $ nf A.encode (decodeA intA)
        , bench "aeson-new-builder" $ nf AUTF8.encode (decodeA intA)
        , bench "json"  $ nf encodeJ (decodeJ intJ)
        ]
      ]
    ]
