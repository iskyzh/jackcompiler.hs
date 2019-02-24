module CodeWriterSpec
    ( testCodeWriter
    )
where
import           Test.Hspec
import           CodeWriter
import           TokenParser
import           Tokenizer
import           Data.Map.Strict

makeLocal x = ST (fromList []) (fromList x) (fromList []) (fromList [])
makeEmpty = ST (fromList []) (fromList []) (fromList []) (fromList [])

testCodeWriter = do
    describe "code writer" $ do
        describe "symbol table" $ do
            it "should recognize field" $ do
                -- classVarDec: field int x, y;
                let parseResult = ParseTree {what = "classVarDec", children = [ParseNode {content = Keyword "field"},ParseNode {content = Keyword "int"},ParseNode {content = Identifier "x"},ParseNode {content = Symbol ','},ParseNode {content = Identifier "y"},ParseNode {content = Symbol ';'}]}
                1 `shouldBe` 1

            it "should recognize static" $ do
                -- classVarDec: static int x, y;
                let parseResult = ParseTree {what = "classVarDec", children = [ParseNode {content = Keyword "static"},ParseNode {content = Keyword "int"},ParseNode {content = Identifier "x"},ParseNode {content = Symbol ','},ParseNode {content = Identifier "y"},ParseNode {content = Symbol ';'}]}
                1 `shouldBe` 1

            it "should recognize var" $ do
                -- varDec: var int x, y;
                let parseResult = ParseTree {what = "varDec", children = [ParseNode {content = Keyword "var"},ParseNode {content = Keyword "int"},ParseNode {content = Identifier "x"},ParseNode {content = Symbol ','},ParseNode {content = Identifier "y"},ParseNode {content = Symbol ';'}]};
                1 `shouldBe` 1

            it "should recognize local" $ do
                -- parameterList: Point a, Point b
                let parseResult = ParseTree {what = "parameterList", children = [ParseNode {content = Identifier "Point"},ParseNode {content = Identifier "a"},ParseNode {content = Symbol ','},ParseNode {content = Identifier "Point"},ParseNode {content = Identifier "b"}]}
                1 `shouldBe` 1

        it "should write term" $ do
            let st = makeLocal [("y", (1, Primitive "int"))]
            -- term: y
            let parseResult = ParseTree {what = "term", children = [ParseNode {content = Identifier "y"}]}
            (writeCode st parseResult) `shouldBe` ["push local 1"]

        it "should write constant term" $ do
            -- term: 1
            let parseResult = ParseTree {what = "term", children = [ParseNode {content = IntegerConstant 1}]}
            (writeCode makeEmpty parseResult) `shouldBe` ["push constant 1"]

        it "should write expression" $ do
            let st = makeLocal  [("x", (1, Primitive "int")), ("y", (2, Primitive "int")), ("z", (3, Primitive "int"))]
            -- term: x + y + z
            let parseResult = ParseTree {what = "expression", children = [ParseTree {what = "term", children = [ParseNode {content = Identifier "x"}]},ParseNode {content = Symbol '+'},ParseTree {what = "term", children = [ParseNode {content = Identifier "y"}]},ParseNode {content = Symbol '+'},ParseTree {what = "term", children = [ParseNode {content = Identifier "z"}]}]}
            (writeCode st parseResult) `shouldBe` ["push local 1", "push local 2", "add", "push local 3", "add"]

        it "should write expression list" $ do
            -- expression list: 1, 2, 3
            let parseResult = ParseTree {what = "expressionList", children = [ParseTree {what = "expression", children = [ParseTree {what = "term", children = [ParseNode {content = IntegerConstant 1}]}]},ParseNode {content = Symbol ','},ParseTree {what = "expression", children = [ParseTree {what = "term", children = [ParseNode {content = IntegerConstant 2}]}]},ParseNode {content = Symbol ','},ParseTree {what = "expression", children = [ParseTree {what = "term", children = [ParseNode {content = IntegerConstant 3}]}]}]}
            (writeCode makeEmpty parseResult) `shouldBe` ["push constant 1", "push constant 2", "push constant 3"]

        it "should write return" $ do
            -- return Math.sqrt((dx*dx)+(dy*dy));
            let parseResult = ParseTree {what = "returnStatement", children = [ParseNode {content = Keyword "return"},ParseTree {what = "expression", children = [ParseTree {what = "term", children = [ParseNode {content = Identifier "Math"},ParseNode {content = Symbol '.'},ParseNode {content = Identifier "sqrt"},ParseNode {content = Symbol '('},ParseTree {what = "expressionList", children = [ParseTree {what = "expression", children = [ParseTree {what = "term", children = [ParseNode {content = Symbol '('},ParseTree {what = "expression", children = [ParseTree {what = "term", children = [ParseNode {content = Identifier "dx"}]},ParseNode {content = Symbol '*'},ParseTree {what = "term", children = [ParseNode {content = Identifier "dx"}]}]},ParseNode {content = Symbol ')'}]},ParseNode {content = Symbol '+'},ParseTree {what = "term", children = [ParseNode {content = Symbol '('},ParseTree {what = "expression", children = [ParseTree {what = "term", children = [ParseNode {content = Identifier "dy"}]},ParseNode {content = Symbol '*'},ParseTree {what = "term", children = [ParseNode {content = Identifier "dy"}]}]},ParseNode {content = Symbol ')'}]}]}]},ParseNode {content = Symbol ')'}]}]},ParseNode {content = Symbol ';'}]}
            1 `shouldBe` 1