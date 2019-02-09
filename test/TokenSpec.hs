module TokenSpec where

import HuskPrelude
import Data.Result
import Test.Hspec
import Encdec.Encoder as Encoder
import Encdec.Decoder as Decoder
import Encdec.Encoded
import Encdec.Encoding
import Crypto.Payload
import Crypto.Token as Token
import Crypto.Sign.Ed25519 (PublicKey(..), SecretKey(..))
import Data.Time.Clock.POSIX (getPOSIXTime)


spec :: Spec
spec = do
  let payload = Payload "sub" 1549699848 2550827138
      payloadExp = Payload "sub" 0 1549699848
      payloadIat = Payload "sub" 2550827138 2550827138
      payloadExpIat = Payload "sub" 2550827139 2550827138
      pk = PublicKey "\142\129F?K\234jzl?v\226\161\&36?RS\161\SOe\216v\250\191\219\187o'G\234\148"
      sk = SecretKey "\130\241\145\247\156CaSU|\183V\f\144\231\196\NAK\197\131\128e\158\187e\238_\239\149\147\170\179r\142\129F?K\234jzl?v\226\161\&36?RS\161\SOe\216v\250\191\219\187o'G\234\148"
      tokenInv = Token "eyJzdWIiOiJzdWIiLCJleHAiOjI1NTA4MjcxMzgsImlhdCI6MTU0OTY5OTg0OH0.aItkm0gxmJ-K1oLkJ88flhW-5J9aiqltdeXLqH_p74Hx4J253mSwHPxA1HgmsBKybjvTaU2MWD0oQkcsIP9o5Ag"

  time <- round <$> runIO getPOSIXTime

  describe "split token" $ do
    it "conform examples" $ do
      (Token "eyJzdWIi.lSc29Gv1-2Ag" |> Token.splitToken) `shouldBe` (Ok (Encoded "eyJzdWIi", Encoded "lSc29Gv1-2Ag"))
      (Token "eyJzdWIilSc29Gv1-2Ag" |> Token.splitToken) `shouldBe` Err "Malformed token"
      (Token "eyJ...zdWIilSc29Gv1-2Ag" |> Token.splitToken) `shouldBe` Err "Malformed token"
      (Token "eyJ.zdWI.ilSc29Gv1-2Ag" |> Token.splitToken) `shouldBe` Err "Malformed token"

  describe "sign verify" $ do
    it "conform examples" $ do
      Token.sign sk payload
        `shouldBe`
        (Token "eyJzdWIiOiJzdWIiLCJleHAiOjI1NTA4MjcxMzgsImlhdCI6MTU0OTY5OTg0OH0.Itkm0gxmJ-K1oLkJ88flhW-5J9aiqltdeXLqH_p74Hx4J253mSwHPxA1HgmsBKybjvTaU2MWD0oQkcsIP9o5Ag")
      (Token.sign sk payload |> Token.verify pk time) `shouldBe` Ok (Token.sign sk payload)
      Token.verify pk time tokenInv `shouldBe` Err "Token is invalid"
      (Token.sign sk payloadExp |> Token.verify pk time) `shouldBe` Err "Token has expired"
      (Token.sign sk payloadIat |> Token.verify pk time) `shouldBe` Err "Token not active"
      (Token.sign sk payloadExpIat |> Token.verify pk time) `shouldBe` Err "Malformed token"
      Token.verify pk time (Token "tok.en") `shouldBe` Err "Decoding from JSON to Payload error"
