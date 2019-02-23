module CodeWriterSpec
    ( testCodeWriter
    )
where
import           Test.Hspec
import           CodeWriter
import           TokenParser
import           Tokenizer
import           Data.Map.Strict

testCodeWriter = do
    describe "code writer" $ do
        it "should write term" $ do
            let st = ST { local = fromList [("y", 1)]
                            }
            -- term: y
            let parseResult = ParseTree {what = "term", children = [ParseNode {content = Identifier "y"}]}
            (writeCode st parseResult) `shouldBe` ["push local 1"]