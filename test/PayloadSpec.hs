module PayloadSpec where

import HuskPrelude
import Data.Result
import Test.Hspec
import Encdec.Encoder as Encoder
import Encdec.Decoder as Decoder
import Encdec.Encoded
import Encdec.Encoding
import Crypto.Payload as Payload

spec :: Spec
spec = do
  describe "encode" $ do
    it "conform examples" $ do
      Encoder.encode (Payload "sub" 0 0) `shouldBe` (Encoded "{\"sub\":\"sub\",\"exp\":0,\"iat\":0}" :: Encoded JSON)

  describe "decoder" $ do
    it "conform examples" $ do
      Decoder.decode (Encoded "{\"sub\":\"sub\",\"exp\":0,\"iat\":0}" :: Encoded JSON) `shouldBe` (Ok (Payload "sub" 0 0) :: Result Payload)
