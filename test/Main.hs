import Test.Tasty
import Test.Tasty.HUnit
import ParseInput
import Types

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "ParseInput"
  [ testCase "parse valid gender line" $
      _parseGender ["Alice M"] @?= Right [("Alice", Male)]
  , testCase "parse valid graph returns vertices" $
      case parseInput "Alice M\n\n\n" of
        Right vs -> length vs @?= 1
        Left _   -> assertFailure "expected Right"
  -- more tests...
  ]
