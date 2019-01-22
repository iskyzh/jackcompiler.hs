import           Test.Hspec
import           TokenizerSpec
import           TokenParserSpec

main :: IO ()
main = hspec $ do
    testTokenizer
    testTokenParser
    