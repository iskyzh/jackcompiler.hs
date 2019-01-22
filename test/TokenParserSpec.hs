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
        it "should parse term" $ do
            (parse term [Identifier "testVar"]) `shouldBe`  [(ParseNode "varName" $ Identifier "testVar",[])]
            (parse term [IntegerConstant 2333]) `shouldBe`  [(ParseNode "integerConstant" $ IntegerConstant 2333,[])]
            (parse term [StringConstant "testStr"]) `shouldBe`  [(ParseNode "stringConstant" $ StringConstant "testStr",[])]
        it "should parse expression" $ do
            (parse expression [Identifier "testVar", Symbol '+', Identifier "testVar2"]) 
                `shouldBe`  [(ParseResult "term" $ [ParseNode "varName" $ Identifier "testVar", ParseNode "op" $ Symbol '+', ParseNode "varName" $ Identifier "testVar2"],[])]
            (parse expression [Identifier "testVar", Symbol '+', Identifier "testVar2", Symbol '|', Identifier "testVar3"]) 
                `shouldBe`  [(ParseResult "term" $ [
                    ParseNode "varName" $ Identifier "testVar", 
                    ParseNode "op" $ Symbol '+', 
                    ParseNode "varName" $ Identifier "testVar2", 
                    ParseNode "op" $ Symbol '|', 
                    ParseNode "varName" $ Identifier "testVar3"],[])]
                    