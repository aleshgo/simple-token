module Crypto.Token where

import HuskPrelude
import Data.Result
import Crypto.Payload
import Crypto.Sign.Ed25519
  ( dsign
  , dverify
  , PublicKey(..)
  , SecretKey(..)
  , Signature(..)
  )
import Encdec.Utils as Utils
import Encdec.Encoder as Encoder
import Encdec.Decoder as Decoder
import Encdec.Encoded
import Encdec.Encoding
import Data.ByteString (split)

newtype Token = Token
  { unToken :: ByteString
  } deriving (Eq, Show)

sign :: SecretKey -> Payload -> Token
sign sk payload =
  let
    payloadEnc :: Encoded Base64Url
    payloadEnc =
      (Encoder.encode payload :: Encoded JSON)
        |> Encoder.encode . encoded
        |> Utils.unpad
    signatureEnc :: Encoded Base64Url
    signatureEnc =
      dsign sk (encoded payloadEnc)
        |> unSignature
        |> Encoder.encode
        |> Utils.unpad
  in Token (encoded payloadEnc <> "." <> encoded signatureEnc)

splitToken :: Token -> Maybe (Encoded Base64Url, Encoded Base64Url)
splitToken (Token str) =
  let
     splitStr = split 46 str
     toPair [x, y] = (Encoded x, Encoded y)
  in
    if length splitStr /= 2
       then Nothing
       else Just (toPair splitStr)

verify :: PublicKey -> Token -> Bool
verify pk token =
  case splitToken token of
     Nothing -> False
     Just (payloadEnc, signatureEnc) ->
       let
        signature =
          Utils.pad signatureEnc
           |> Decoder.decode
       in
         result (const False) ((dverify pk (encoded payloadEnc)) . Signature) signature

