{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Crypto.Payload where

import HuskPrelude
import Data.Result
import GHC.Generics (Generic)
import Encdec.Encoder
import Encdec.Decoder
import Encdec.Encoded
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Aeson as Aeson
import Data.Time.Clock.POSIX (getPOSIXTime)

data JSON
data JSONBase64

data Payload = Payload
  { sub :: Text
  , iat :: Int
  , exp :: Int
  } deriving (Eq, Show, Generic)
instance Aeson.ToJSON Payload
instance Aeson.FromJSON Payload

payload :: Text -> Int -> IO Payload
payload sub exp =
  ((\time -> Payload sub time (time + exp)) . round) <$> getPOSIXTime

instance Encoder Payload (Encoded JSON) where
  encode = Encoded . BSL.toStrict . Aeson.encode

instance Decoder (Encoded JSON) Payload where
  decode (Encoded a) =
    a |> Aeson.eitherDecode . BSL.fromStrict
      |> either (leftToErr "Decoding from JSON to Payload error") Ok

verify :: Int -> Payload -> Result Payload
verify time payload@(Payload _ iat exp)
  | iat > exp = Err "Malformed token"
  | iat > time = Err "Token not active"
  | exp <= time = Err "Token has expired"
  | otherwise = Ok payload
