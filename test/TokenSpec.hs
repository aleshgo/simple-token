module TokenSpec where

import Test.Hspec
import Encdec.Types
import Encdec.Encoder as Encoder
import Encdec.Decoder as Decoder
import Crypto.Payload
import Crypto.Token as Token
import Data.ByteString (ByteString)
import Crypto.Sign.Ed25519 (PublicKey(..), SecretKey(..))


spec :: Spec
spec = do
  let payload = Payload "sub" 0 0
  let pk = PublicKey "\142\129F?K\234jzl?v\226\161\&36?RS\161\SOe\216v\250\191\219\187o'G\234\148"
  let sk = SecretKey "\130\241\145\247\156CaSU|\183V\f\144\231\196\NAK\197\131\128e\158\187e\238_\239\149\147\170\179r\142\129F?K\234jzl?v\226\161\&36?RS\161\SOe\216v\250\191\219\187o'G\234\148"

  describe "split token" $ do
    it "conform examples" $ do
      (Token "eyJzdWIi.lSc29Gv1-2Ag" |> Token.splitToken) `shouldBe` (Just (Encoded "eyJzdWIi", Encoded "lSc29Gv1-2Ag"))
      (Token "eyJzdWIilSc29Gv1-2Ag" |> Token.splitToken) `shouldBe` Nothing
      (Token "eyJ...zdWIilSc29Gv1-2Ag" |> Token.splitToken) `shouldBe` Nothing
      (Token "eyJ.zdWI.ilSc29Gv1-2Ag" |> Token.splitToken) `shouldBe` Nothing

  describe "sign verify" $ do
    it "conform examples" $ do
      Token.sign sk payload
        `shouldBe`
        (Token "eyJzdWIiOiJzdWIiLCJleHAiOjAsImlhdCI6MH0.lSc29Gv1-2MNsvPEje_UwZATDIgBFzlrZmqX7Lq-9_jW7rj4zl782VyzhbI4rn2aTILnskbmiuLHN5A7ZpUOAg")
      (Token.sign sk payload |> Token.verify pk) `shouldBe` True
      Token.verify pk (Token "tok.en") `shouldBe` False
