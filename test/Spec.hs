import           Test.Hspec
import           TokenizerSpec
import           TokenParserSpec
import           CodeWriterSpec

main :: IO ()
main = hspec $ do
    testTokenizer
    testTokenParser
    testCodeWriter