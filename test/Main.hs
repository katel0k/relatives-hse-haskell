import ParseInput
import Test.Tasty
import Test.Tasty.HUnit
import Types

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "ParseInput"
    [ testGroup
        "gender format"
        [ testCase "parse full graph" $
            case parseInput "Alice (Ж)\nBob (М)\n\n" of
              Right vs -> do
                length vs @?= 2
                let names = map name vs
                assertBool "Alice present" ("Alice" `elem` names)
                assertBool "Bob present" ("Bob" `elem` names)
                gender (head $ filter (\v -> name v == "Alice") vs) @?= Female
                gender (head $ filter (\v -> name v == "Bob") vs) @?= Male
              Left e -> assertFailure ("expected Right, got " ++ errorMessage e)
        ],
      testGroup
        "comments"
        [ testCase "comment line in gender section is ignored, two persons parsed" $
            case parseInput "A (М)\n# ignore this\nB (Ж)\n\n\n" of
              Right vs -> length vs @?= 2
              Left e -> assertFailure ("expected Right: " ++ errorMessage e)
        ],
      testGroup
        "section order (marriages before parent-child)"
        [ testCase "full parse: gender, then marriages, then parent-child" $
            case parseInput fullExample of
              Right vs -> do
                length vs @?= 3
                let names = map name vs
                assertBool "Alice present" ("Alice" `elem` names)
                assertBool "Bob present" ("Bob" `elem` names)
                assertBool "Child present" ("Child" `elem` names)
              Left e -> assertFailure ("expected Right: " ++ errorMessage e)
        ],
      testGroup
        "invalid input"
        [ testCase "invalid gender line gives Left" $
            case _parseGender ["Alice basically_any_gibberish"] of
              Left _ -> pure ()
              Right _ -> assertFailure "expected Left for old format"
        ]
    ]

-- Gender section, then marriages (A <-> B), then parent-child (Alice -> Child, Bob -> Child).
fullExample :: String
fullExample =
  unlines
    [ "Alice (Ж)",
      "Bob (М)",
      "Child (Ж)",
      "",
      "Alice <-> Bob",
      "",
      "Alice -> Child",
      "Bob -> Child"
    ]
