
module TokenizerSpec
    ( testTokenizer
    )
where
import           Test.Hspec
import           Tokenizer
import           Control.Exception              ( evaluate )

testTokenizer = do
    describe "tokenizer" $ do
        it "should parse symbols" $ do
            tokenize "&,;)[=-*+].|<{>-}(/"
                `shouldBe` [ Symbol '&'
                           , Symbol ','
                           , Symbol ';'
                           , Symbol ')'
                           , Symbol '['
                           , Symbol '='
                           , Symbol '-'
                           , Symbol '*'
                           , Symbol '+'
                           , Symbol ']'
                           , Symbol '.'
                           , Symbol '|'
                           , Symbol '<'
                           , Symbol '{'
                           , Symbol '>'
                           , Symbol '-'
                           , Symbol '}'
                           , Symbol '('
                           , Symbol '/'
                           ]

        it "should parse integer constant" $ do
            tokenize "23333" `shouldBe` [IntegerConstant 23333]
            tokenize "3" `shouldBe` [IntegerConstant 3]
            tokenize "0" `shouldBe` [IntegerConstant 0]
            tokenize "02333" `shouldBe` [IntegerConstant 2333]

        it "should not parse wrong integer constant" $ do
            evaluate (tokenize "2333ab") `shouldThrow` anyErrorCall

        it "should parse string constant" $ do
            tokenize "\"23333\"" `shouldBe` [StringConstant "23333"]
            tokenize "\"3\"" `shouldBe` [StringConstant "3"]
            tokenize "\"Hello, World!\""
                `shouldBe` [StringConstant "Hello, World!"]

        it "should not parse wrong string constant" $ do
            evaluate (tokenize "\"2333\n\"") `shouldThrow` anyErrorCall
            evaluate (tokenize "\"2333") `shouldThrow` anyErrorCall

        it "should parse identifier" $ do
            tokenize "a" `shouldBe` [Identifier "a"]
            tokenize "abcd" `shouldBe` [Identifier "abcd"]
            tokenize "_a" `shouldBe` [Identifier "_a"]
            tokenize "___abcd" `shouldBe` [Identifier "___abcd"]
            tokenize "c23333" `shouldBe` [Identifier "c23333"]

        it "should parse keyword" $ do
            tokenize "class" `shouldBe` [Keyword "class"]
            tokenize "constructor" `shouldBe` [Keyword "constructor"]
            tokenize "function" `shouldBe` [Keyword "function"]
            tokenize "method" `shouldBe` [Keyword "method"]
            tokenize "field" `shouldBe` [Keyword "field"]
            tokenize "static" `shouldBe` [Keyword "static"]
            tokenize "var" `shouldBe` [Keyword "var"]
            tokenize "int" `shouldBe` [Keyword "int"]
            tokenize "char" `shouldBe` [Keyword "char"]
            tokenize "boolean" `shouldBe` [Keyword "boolean"]
            tokenize "void" `shouldBe` [Keyword "void"]
            tokenize "true" `shouldBe` [Keyword "true"]
            tokenize "false" `shouldBe` [Keyword "false"]
            tokenize "null" `shouldBe` [Keyword "null"]
            tokenize "this" `shouldBe` [Keyword "this"]
            tokenize "let" `shouldBe` [Keyword "let"]
            tokenize "do" `shouldBe` [Keyword "do"]
            tokenize "if" `shouldBe` [Keyword "if"]
            tokenize "else" `shouldBe` [Keyword "else"]
            tokenize "while" `shouldBe` [Keyword "while"]
            tokenize "return" `shouldBe` [Keyword "return"]

        it "should parse multiple tokens" $ do
            tokenize "let    x=x+ 1"
                `shouldBe` [ Keyword "let"
                           , Identifier "x"
                           , Symbol '='
                           , Identifier "x"
                           , Symbol '+'
                           , IntegerConstant 1
                           ]
            tokenize "do    brush. print(   obj, 2  , 200 , true)"
                `shouldBe` [ Keyword "do"
                           , Identifier "brush"
                           , Symbol '.'
                           , Identifier "print"
                           , Symbol '('
                           , Identifier "obj"
                           , Symbol ','
                           , IntegerConstant 2
                           , Symbol ','
                           , IntegerConstant 200
                           , Symbol ','
                           , Keyword "true"
                           , Symbol ')'
                           ]
            tokenize "if ((x + size) < 510) { }\nreturn;"
                `shouldBe` [ Keyword "if"
                           , Symbol '('
                           , Symbol '('
                           , Identifier "x"
                           , Symbol '+'
                           , Identifier "size"
                           , Symbol ')'
                           , Symbol '<'
                           , IntegerConstant 510
                           , Symbol ')'
                           , Symbol '{'
                           , Symbol '}'
                           , Keyword "return"
                           , Symbol ';'
                           ]

        it "should ignore comments" $ do
            tokenize "// comments" `shouldBe` []
            tokenize "/* comments \n\n */" `shouldBe` []
            tokenize
                    "if ((x + size) < 510) { /* \n SOME COMMENTS \n*/ }\n // comments \n return;"
                `shouldBe` [ Keyword "if"
                           , Symbol '('
                           , Symbol '('
                           , Identifier "x"
                           , Symbol '+'
                           , Identifier "size"
                           , Symbol ')'
                           , Symbol '<'
                           , IntegerConstant 510
                           , Symbol ')'
                           , Symbol '{'
                           , Symbol '}'
                           , Keyword "return"
                           , Symbol ';'
                           ]
