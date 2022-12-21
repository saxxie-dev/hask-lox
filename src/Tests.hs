import Test.HUnit
import Scanner
import Text.Parsec
import Text.Parsec.String


testBasicToken :: Test 
testBasicToken = TestCase $ assertEqual
  "should parse basic tokens"
  (Right PLUS)
  (parse basicToken [] "+")

testStringLiteralToken :: Test
testStringLiteralToken = TestCase $ assertEqual
  "should parse basic string literal token"
  (Right (STRING "Hello World"))
  (parse literalToken [] "\"Hello World\"")
-- testTokenize :: Test
-- testTokenize = TestCase $ assertEqual
--   "tokenize should return the expected result for a simple list of tokens"
--   (Right [CLASS, NUMBER "2d", NUMBER "2", IF])
--   (parse tokenize [] "class 2 2 if")

-- testTokenizeComments :: Test
-- testTokenizeComments = TestCase $ assertEqual
--   "tokenize should return the expected result for a simple list of tokens in the presence of line comments"
--   (Right [CLASS, NUMBER "2d", NUMBER "2", IF])
--   (parse tokenize [] "class 2 2 if")

tests :: IO [Test]
tests = return [
  TestLabel "test basic token" testBasicToken,
  TestLabel "test basic string literal token" testStringLiteralToken
  ]
  -- TestLabel "test 1" testTokenize,
  -- TestLabel "test 2" testTokenizeComments]

main :: IO ()
main = do
  testList <- (TestList <$> tests)
  counts <- runTestTT testList
  putStrLn (show counts)
