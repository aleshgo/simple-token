module PayloadSpec where

import HuskPrelude
import Data.Result
import Test.Hspec
import Encdec.Encoder as Encoder
import Encdec.Decoder as Decoder
import Encdec.Encoded
import Encdec.Encoding
import Crypto.Payload as Payload
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Monad.IO.Class (liftIO)

spec :: Spec
spec = do
  let payload = Payload "sub" 1549699848 2550827138
      payloadExp = Payload "sub" 0 1549699848
      payloadIat = Payload "sub" 2550827138 2550827138
      payloadExpIat = Payload "sub" 2550827139 2550827138

  time <- round <$> runIO getPOSIXTime

  describe "encode" $ do
    it "conform examples" $ do
      Encoder.encode (Payload "sub" 0 0) `shouldBe` (Encoded "{\"sub\":\"sub\",\"exp\":0,\"iat\":0}" :: Encoded JSON)

  describe "decoder" $ do
    it "conform examples" $ do
      Decoder.decode (Encoded "{\"sub\":\"sub\",\"exp\":0,\"iat\":0}" :: Encoded JSON) `shouldBe` (Ok (Payload "sub" 0 0) :: Result Payload)

  describe "verify" $ do
    it "conform examples" $ do
      Payload.verify time payload `shouldBe` Ok payload
      Payload.verify time payloadExp `shouldBe` Err "Token has expired"
      Payload.verify time payloadIat `shouldBe` Err "Token not active"
      Payload.verify time payloadExpIat `shouldBe` Err "Malformed token"
