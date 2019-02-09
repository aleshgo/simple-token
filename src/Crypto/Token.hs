module Crypto.Token where

import HuskPrelude
import Data.Result
import Crypto.Payload as Payload
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

splitToken :: Token -> Result (Encoded Base64Url, Encoded Base64Url)
splitToken (Token str)
  | length splitStr == 2 = Ok $ toPair splitStr
  | otherwise = Err "Malformed token"
  where
     splitStr = split 46 str
     toPair [x, y] = (Encoded x, Encoded y)

verify :: PublicKey -> Int -> Token -> Result Token
verify pk time token = do
  (payloadEnc, signatureEnc) <- splitToken token
  payload <- (Utils.pad payloadEnc |> Decoder.decode :: Result ByteString) >>= (\x-> Decoder.decode $ (Encoded x :: Encoded JSON)) :: Result Payload
  Payload.verify time payload
  signature <- Utils.pad signatureEnc |> Decoder.decode :: Result ByteString
  if dverify pk (encoded payloadEnc) (Signature signature)
    then Ok token
    else Err "Token is invalid"
