module Data.Bitcoin.BlockSpec where

import qualified Data.HexString as HS

import           Data.Bitcoin.Block
import qualified Data.ByteString.Char8          as BS8 (pack)

import           Test.Hspec

spec :: Spec
spec = do
  describe "when parsing a specific block" $ do
    let hex = HS.hexString $ BS8.pack "03000000f4c7237cbb549c2354cc62c9a668af38f6fefd40ad47f1e4c74ad582ae9b1f58f928b445a7129277f55c69cbe27dc0f624f652c4d2f31f1840e2bae2fdcb4489d30a3e55ffff7f20020000000201000000010000000000000000000000000000000000000000000000000000000000000000ffffffff0b01690101062f503253482fffffffff011019062a01000000232102ef5396e5e464f62fbe1a865b8f8469b2d43036dbbf781e5318dfc04bbdcae8fbac0000000001000000038f3021ca346d0a2404c33b6ea3aa5a0dd0acd21507c411a92467f919851a7b1e0000000049483045022100ac8b2c8645b7ad344a94ed1a4b8b5cd32fec2c27d75719852b6459f25b939a9b022056e7d49efe036fb1fc93773587972cbc8e8bf06463f6d170ce128226b025228c01ffffffff8eec8c01a8fe1bb3a265363d4a3863326f79d98d0fb05def21e0f13c4cb70853000000006b483045022100d9124cd8ec46bbda359e41fa92c9a5ad142ce42239e1ac12cc22ae191609ec7702205974c9c9ff7578e86928693c65fd817fcfbcd6bfcce2424695b9fff5f2c810ed012102c36b6125cfbb9e50bf5c390f948640765e3b0fb980c313c9307692d2d2b8b62dffffffff57f74f9754e5c9814e928cd52bfffd74e7a29a2b35bf0444ddbe69f3de9268dc0000000049483045022100f539d6202f1025b1abbf38343d9e6a2570db19684b50351ba5e87f534419721302207b45ef08f07095c19012969e1335e12c18b1efff3e0deb83b1010c110c49c2bb01ffffffff01089d17a8040000001976a914c09c37f829a24e0c61f809f1c3c73f78f71aa2ae88ac00000000"

    it "encoding a decoding a block results in the original hex" $
      (encode . decode) hex `shouldBe` hex

    it "succesfully parses a block into a meaningful object" $ do
      let decoded = decode hex

      case decoded of
       (Block (BlockHeader 3 _ _ 1430129363 _ 2) _ _) -> return ()
       _                                              -> expectationFailure ("Result does not match expected: " ++ show decoded)

    it "succesfully generates hash of parsed object" $ do
      headerHash (decode hex) `shouldBe` (HS.hexString $ BS8.pack "269c6a72b865805c74260c9d99b74cf2f20d9fccd6c3749e607427102dd1edda")
