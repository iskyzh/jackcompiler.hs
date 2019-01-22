module TokenParserSpec (
    testTokenParser
) where
import           Test.Hspec
import           Tokenizer
import           TokenParser
import           Control.Exception              ( evaluate )

testTokenParser = do
    describe "expressions" $ do
        it "should parse keyword constant" $ do
            (parse keywordConstant [Keyword "true"]) `shouldBe` [(Keyword "true",[])]
            (parse keywordConstant [Keyword "false"]) `shouldBe` [(Keyword "false",[])]
            (parse keywordConstant [Keyword "null"]) `shouldBe` [(Keyword "null",[])]
            (parse keywordConstant [Keyword "this"]) `shouldBe` [(Keyword "this",[])]
