module TokenParserSpec (
    testTokenParser
) where
import           Test.Hspec
import           Tokenizer
import           TokenParser
import           Control.Exception              ( evaluate )
import           ParserProgramStructure

testTokenParser = do
    describe "program structure" $ do
        it "should parse varName" $ do 
            (parse pVarName $ tokenize "testVar1") `shouldBe` [(ParseNode {content = Identifier "testVar1"},[])]
        it "should parse subroutineName" $ do
            (parse pSubroutineName $ tokenize "testFunc1") `shouldBe` [(ParseNode {content = Identifier "testFunc1"},[])]
        it "should parse className" $ do
            (parse pClassName $ tokenize "testCls1") `shouldBe` [(ParseNode {content = Identifier "testCls1"},[])]
        it "should parse classVarDec" $ do
            (parse pClassVarDec $ tokenize "static boolean test;") `shouldBe` [(ParseTree {what = "classVarDec", children = [ParseNode {content = Keyword "static"},ParseNode {content = Keyword "boolean"},ParseNode {content = Identifier "test"},ParseNode {content = Symbol ';'}]},[])]
            (parse pClassVarDec $ tokenize "static boolean test1, test2;") `shouldBe` [(ParseTree {what = "classVarDec", children = [ParseNode {content = Keyword "static"},ParseNode {content = Keyword "boolean"},ParseNode {content = Identifier "test1"},ParseNode {content = Symbol ','},ParseNode {content = Identifier "test2"},ParseNode {content = Symbol ';'}]},[])]
        it "should parse parameterList" $ do
            (parse pParameterList $ tokenize "int Ax") `shouldBe` [(ParseTree {what = "parameterList", children = [ParseNode {content = Keyword "int"},ParseNode {content = Identifier "Ax"}]},[])]
            (parse pParameterList $ tokenize "int Ax, int Ay, int Asize") `shouldBe` [(ParseTree {what = "parameterList", children = [ParseNode {content = Keyword "int"},ParseNode {content = Identifier "Ax"},ParseNode {content = Symbol ','},ParseNode {content = Keyword "int"},ParseNode {content = Identifier "Ay"},ParseNode {content = Symbol ','},ParseNode {content = Keyword "int"},ParseNode {content = Identifier "Asize"}]},[])]
        it "should parse subroutineDec" $ do
            (parse pSubroutineDec $ tokenize "constructor Square new(int Ax, int Ay, int Asize) {}") `shouldBe` [(ParseTree {what = "subroutineDec", children = [ParseNode {content = Keyword "constructor"},ParseNode {content = Identifier "Square"},ParseNode {content = Identifier "new"},ParseNode {content = Symbol '('},ParseTree {what = "parameterList", children = [ParseNode {content = Keyword "int"},ParseNode {content = Identifier "Ax"},ParseNode {content = Symbol ','},ParseNode {content = Keyword "int"},ParseNode {content = Identifier "Ay"},ParseNode {content = Symbol ','},ParseNode {content = Keyword "int"},ParseNode {content = Identifier "Asize"}]},ParseNode {content = Symbol ')'},ParseTree {what = "subroutineBody", children = [ParseNode {content = Symbol '{'},ParseNode {content = Symbol '}'}]}]},[])]
        it "should parse class" $ do
            (parse pClass $ tokenize "class Square { static boolean test; constructor Square new(int Ax, int Ay, int Asize) {} }") `shouldBe` [(ParseTree {what = "class", children = [ParseNode {content = Keyword "class"},ParseNode {content = Identifier "Square"},ParseNode {content = Symbol '{'},ParseTree {what = "classVarDec", children = [ParseNode {content = Keyword "static"},ParseNode {content = Keyword "boolean"},ParseNode {content = Identifier "test"},ParseNode {content = Symbol ';'}]},ParseTree {what = "subroutineDec", children = [ParseNode {content = Keyword "constructor"},ParseNode {content = Identifier "Square"},ParseNode {content = Identifier "new"},ParseNode {content = Symbol '('},ParseTree {what = "parameterList", children = [ParseNode {content = Keyword "int"},ParseNode {content = Identifier "Ax"},ParseNode {content = Symbol ','},ParseNode {content = Keyword "int"},ParseNode {content = Identifier "Ay"},ParseNode {content = Symbol ','},ParseNode {content = Keyword "int"},ParseNode {content = Identifier "Asize"}]},ParseNode {content = Symbol ')'},ParseTree {what = "subroutineBody", children = [ParseNode {content = Symbol '{'},ParseNode {content = Symbol '}'}]}]},ParseNode {content = Symbol '}'}]},[])]
    describe "statements" $ do
        it "should parse statement" $ do
            0 `shouldBe` 0
