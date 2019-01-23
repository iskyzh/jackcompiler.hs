module TokenParserSpec (
    testTokenParser
) where
import           Test.Hspec
import           Tokenizer
import           TokenParser
import           Control.Exception              ( evaluate )
import           ParserProgramStructure
import           ParserStatements
import           ParserUtil
import           ParserExpressions
import           System.Directory               ( getCurrentDirectory
                                                , withCurrentDirectory
                                                , canonicalizePath
                                                )

testTokenParser = do
    describe "parser" $ do
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
                (parse pSubroutineDec $ tokenize "constructor Square new(int Ax, int Ay, int Asize) {}") `shouldBe` [(ParseTree {what = "subroutineDec", children = [ParseNode {content = Keyword "constructor"},ParseNode {content = Identifier "Square"},ParseNode{content = Identifier "new"},ParseNode {content = Symbol '('},ParseTree {what = "parameterList", children = [ParseNode {content = Keyword "int"},ParseNode {content = Identifier "Ax"},ParseNode {content = Symbol ','},ParseNode {content = Keyword "int"},ParseNode {content = Identifier "Ay"},ParseNode {content = Symbol ','},ParseNode {content = Keyword "int"},ParseNode {content = Identifier "Asize"}]},ParseNode {content = Symbol ')'},ParseTree {what = "subroutineBody", children = [ParseNode {content = Symbol '{'},ParseTree {what = "statements", children = []},ParseNode {content = Symbol '}'}]}]},[])]
            it "should parse class" $ do
                (parse pClass $ tokenize "class Square { static boolean test; constructor Square new(int Ax, int Ay, int Asize) {} }") `shouldBe` [(ParseTree {what = "class", children = [ParseNode {content = Keyword "class"},ParseNode {content = Identifier "Square"},ParseNode {content = Symbol '{'},ParseTree {what = "classVarDec", children = [ParseNode {content = Keyword "static"},ParseNode {content = Keyword "boolean"},ParseNode {content = Identifier "test"},ParseNode {content = Symbol ';'}]},ParseTree {what = "subroutineDec", children = [ParseNode {content = Keyword "constructor"},ParseNode {content = Identifier "Square"},ParseNode {content = Identifier "new"},ParseNode {content = Symbol '('},ParseTree {what = "parameterList", children = [ParseNode {content = Keyword "int"},ParseNode {content = Identifier "Ax"},ParseNode {content = Symbol ','},ParseNode {content = Keyword "int"},ParseNode {content = Identifier "Ay"},ParseNode {content = Symbol ','},ParseNode {content = Keyword "int"},ParseNode {content = Identifier "Asize"}]},ParseNode {content = Symbol ')'},ParseTree {what = "subroutineBody", children = [ParseNode {content = Symbol '{'},ParseTree {what = "statements", children = []},ParseNode {content = Symbol '}'}]}]},ParseNode {content =Symbol '}'}]},[])]
            it "should parse varDec" $ do
                (parse pVarDec $ tokenize "var bool test1, test2;") `shouldBe` [(ParseTree {what = "varDec", children = [ParseNode {content = Keyword "var"},ParseNode {content = Identifier "bool"},ParseNode {content = Identifier "test1"},ParseNode {content = Symbol ','},ParseNode {content = Identifier "test2"},ParseNode {content = Symbol ';'}]},[])]
        describe "statements" $ do
            it "should parse doStatement" $ do
                (selectParse (parse pDoStatement $ tokenize "do Output.println();")) `shouldBe` ParseTree {what = "doStatement", children = [ParseNode {content = Keyword "do"},ParseNode {content = Identifier "Output"},ParseNode {content = Symbol '.'},ParseNode {content = Identifier "println"},ParseNode {content = Symbol '('},ParseTree {what = "expressionList", children = []},ParseNode {content = Symbol ')'},ParseNode {content = Symbol ';'}]}
            it "should parse letStatement" $ do
                (selectParse (parse pLetStatement $ tokenize "let game = game;")) `shouldBe` ParseTree {what = "letStatement", children = [ParseNode {content = Keyword "let"},ParseNode {content = Identifier "game"},ParseNode {content = Symbol '='},ParseTree {what = "expression", children = [ParseTree {what = "term", children = [ParseNode {content = Identifier "game"}]}]},ParseNode {content = Symbol ';'}]}
            it "should parse ifStatement" $ do
                (selectParse (parse pIfStatement $ tokenize "if (true) {} else {}")) `shouldBe` ParseTree {what = "ifStatement", children = [ParseNode {content = Keyword "if"},ParseNode {content = Symbol '('},ParseTree {what = "expression",children = [ParseTree {what = "term", children = [ParseNode {content = Keyword "true"}]}]},ParseNode {content = Symbol ')'},ParseNode {content = Symbol '{'},ParseTree {what = "statements", children = []},ParseNode {content = Symbol '}'},ParseNode {content = Keyword "else"},ParseNode {content = Symbol '{'},ParseTree {what = "statements", children = []},ParseNode {content = Symbol '}'}]}
                (selectParse (parse pIfStatement $ tokenize "if (true) {}")) `shouldBe` ParseTree {what = "ifStatement", children = [ParseNode {content = Keyword "if"},ParseNode {content = Symbol '('},ParseTree {what = "expression",children = [ParseTree {what = "term", children = [ParseNode {content = Keyword "true"}]}]},ParseNode {content = Symbol ')'},ParseNode {content = Symbol '{'},ParseTree {what = "statements", children = []},ParseNode {content = Symbol '}'}]}
            it "should parse whileStatement" $ do
                (selectParse (parse pWhileStatement $ tokenize "while (true) {} ")) `shouldBe` ParseTree {what = "whileStatement", children = [ParseNode {content = Keyword "while"},ParseNode {content = Symbol '('},ParseTree {what = "expression", children = [ParseTree {what = "term", children = [ParseNode {content = Keyword "true"}]}]},ParseNode {content = Symbol ')'},ParseNode {content = Symbol '{'},ParseTree {what = "statements", children = []},ParseNode {content = Symbol '}'}]}
            it "should parse returnStatement" $ do
                (selectParse (parse pReturnStatement $ tokenize "return ;")) `shouldBe` ParseTree {what = "returnStatement", children = [ParseNode {content = Keyword "return"},ParseNode {content = Symbol ';'}]}
                (selectParse (parse pReturnStatement $ tokenize "return thisVar ;")) `shouldBe` ParseTree {what = "returnStatement", children = [ParseNode {content = Keyword "return"},ParseTree {what = "expression", children = [ParseTree {what = "term", children = [ParseNode {content = Identifier "thisVar"}]}]},ParseNode {content = Symbol ';'}]}
        describe "util" $ do
            it "should parse ()" $ do
                (parse (pBracket '(' ')' (ParseNode <$> sat (== Identifier "a"))) $ tokenize "(a)") `shouldBe` [([ParseNode {content = Symbol '('},ParseNode {content = Identifier "a"},ParseNode {content = Symbol ')'}],[])]
            it "should parse {}" $ do
                (parse (pBracket '{' '}' (ParseNode <$> sat (== Identifier "a"))) $ tokenize "{a}") `shouldBe` [([ParseNode {content = Symbol '{'},ParseNode {content = Identifier "a"},ParseNode {content = Symbol '}'}],[])]
        describe "expressions" $ do
            it "should parse keywordConstant" $ do
                (parse pKeywordConstant $ tokenize "null") `shouldBe` [(ParseNode {content = Keyword "null"},[])]
                (parse pKeywordConstant $ tokenize "true") `shouldBe` [(ParseNode {content = Keyword "true"},[])]
                (parse pKeywordConstant $ tokenize "false") `shouldBe` [(ParseNode {content = Keyword "false"},[])]
                (parse pKeywordConstant $ tokenize "this") `shouldBe` [(ParseNode {content = Keyword "this"},[])]
            it "should parse op" $ do
                (parse pOp $ tokenize "+") `shouldBe` [(ParseNode {content = Symbol '+'},[])]
                (parse pOp $ tokenize "~") `shouldBe` []
            it "should parse unaryOp" $ do
                (parse pUnaryOp $ tokenize "~") `shouldBe` [(ParseNode {content = Symbol '~'},[])]
            it "should parse subroutineCall" $ do
                (parse pSubroutineCall $ tokenize "func()") `shouldBe` [([ParseNode {content = Identifier "func"},ParseNode {content = Symbol '('},ParseTree {what = "expressionList", children = []},ParseNode {content = Symbol ')'}],[])]
                (parse pSubroutineCall $ tokenize "cls.func()") `shouldBe` [([ParseNode {content = Identifier "cls"},ParseNode {content = Symbol '.'},ParseNode {content = Identifier "func"},ParseNode {content = Symbol '('},ParseTree {what = "expressionList", children = []},ParseNode {content = Symbol ')'}],[])]
                (parse pSubroutineCall $ tokenize "func(a, b, c)") `shouldBe` [([ParseNode {content = Identifier "func"},ParseNode {content = Symbol '('},ParseTree {what = "expressionList", children = [ParseTree {what = "expression", children = [ParseTree {what = "term", children = [ParseNode {content = Identifier "a"}]}]},ParseNode {content = Symbol ','},ParseTree {what = "expression", children = [ParseTree {what = "term", children = [ParseNode {content = Identifier "b"}]}]},ParseNode {content = Symbol ','},ParseTree {what = "expression", children = [ParseTree {what = "term", children = [ParseNode {content = Identifier "c"}]}]}]},ParseNode {content = Symbol ')'}],[])]
            it "should parse expression" $ do
                (selectParse (parse pExpression $ tokenize "func() + 233")) `shouldBe` ParseTree {what = "expression", children = [ParseTree {what = "term", children = [ParseNode {content = Identifier "func"},ParseNode {content = Symbol '('},ParseTree {what = "expressionList", children = []},ParseNode {content = Symbol ')'}]},ParseNode {content = Symbol '+'},ParseTree {what = "term", children = [ParseNode {content = IntegerConstant 233}]}]}
                (selectParse (parse pExpression $ tokenize "23333 + 233")) `shouldBe` ParseTree {what = "expression", children = [ParseTree {what = "term", children = [ParseNode {content = IntegerConstant 23333}]},ParseNode {content = Symbol '+'},ParseTree {what = "term", children = [ParseNode {content = IntegerConstant 233}]}]}
            it "should parse term" $ do
                (parse pTerm $ tokenize "233") `shouldBe` [(ParseTree {what = "term", children = [ParseNode {content = IntegerConstant 233}]},[])]
                (parse pTerm $ tokenize "\"23333\"") `shouldBe` [(ParseTree {what = "term", children = [ParseNode {content = StringConstant "23333"}]},[])]
                (parse pTerm $ tokenize "true") `shouldBe` [(ParseTree {what = "term", children = [ParseNode {content = Keyword "true"}]},[])]
                (parse pTerm $ tokenize "thisVar") `shouldBe` [(ParseTree {what = "term", children = [ParseNode {content = Identifier "thisVar"}]},[])]
        describe "should parse Nand2Tetris examples" $ do
            it "should parse ArrayTest" $ do
                testCompileFile "10/ArrayTest/Main"
            it "should parse ExpressionLessSquare" $ do
                testCompileFile "10/ExpressionLessSquare/Main"
                testCompileFile "10/ExpressionLessSquare/Square"
                testCompileFile "10/ExpressionLessSquare/SquareGame"
            it "should parse Square" $ do
                testCompileFile "10/Square/Main"
                testCompileFile "10/Square/Square"
                testCompileFile "10/Square/SquareGame"

compile :: String -> String
compile jack = parseXML (parse pClass $ tokenize jack)

testCompileFile filePath = do
    currentDir <- getCurrentDirectory
    file       <- canonicalizePath (currentDir ++ "/test/data/" ++ filePath)
    jack       <- readFile $ file ++ ".jack"
    result     <- readFile $ file ++ ".xml"
    lines (compile jack) `shouldBe` map (\r -> filter (\c -> c /= '\n' && c /= '\r') r) (lines result)
        