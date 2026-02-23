import Data.List (sort)
import qualified Data.Map.Strict as Map
import ParseInput
import Run
import Rules (rules)
import Test.Tasty
import Test.Tasty.HUnit
import Types (Gender (..), Graph, name, gender, errorMessage)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "All"
    [ testGroup "parsing input" parsingTests,
      testGroup "rules evaluation" rulesTests
    ]

parsingTests :: [TestTree]
parsingTests =
  [ testCase "parse full graph (gender format)" $
      case parseInput "Alice (Ж)\nBob (М)\n\n" of
        Right vs -> do
          length vs @?= 2
          let names = map name vs
          assertBool "Alice present" ("Alice" `elem` names)
          assertBool "Bob present" ("Bob" `elem` names)
          gender (head $ filter ((== "Alice") . name) vs) @?= Female
          gender (head $ filter ((== "Bob") . name) vs) @?= Male
        Left e -> assertFailure ("expected Right, got " ++ errorMessage e),
    testCase "comment line in gender section is ignored" $
      case parseInput "A (М)\n# ignore this\nB (Ж)\n\n\n" of
        Right vs -> length vs @?= 2
        Left e -> assertFailure ("expected Right: " ++ errorMessage e),
    testCase "section order: gender, then marriages, then parent-child" $
      case parseInput fullExample of
        Right vs -> do
          length vs @?= 3
          let names = map name vs
          assertBool "Alice present" ("Alice" `elem` names)
          assertBool "Bob present" ("Bob" `elem` names)
          assertBool "Child present" ("Child" `elem` names)
        Left e -> assertFailure ("expected Right: " ++ errorMessage e),
    testCase "invalid gender line gives Left" $
      case _parseGender ["Alice basically_any_gibberish"] of
        Left _ -> pure ()
        Right _ -> assertFailure "expected Left for invalid gender"
  ]

rulesTests :: [TestTree]
rulesTests =
  [ testCase "rules test input parses to expected graph" $
      assertParsedRulesGraph rulesGraphEve expectedRulesGraphShape,
    testCase "Eve's relatives match expected name->roles map" $
      assertEveRelativesMatch
  ]

-- Assert that rules test input parses and has the expected shape (vertex count, entry present).
assertParsedRulesGraph :: String -> (Graph -> Assertion) -> Assertion
assertParsedRulesGraph input assertShape =
  case parseInput input of
    Left e -> assertFailure ("rules test input failed to parse: " ++ errorMessage e)
    Right graph -> assertShape graph

expectedRulesGraphShape :: Graph -> Assertion
expectedRulesGraphShape graph = do
  length graph @?= 10
  let names = map name graph
  assertBool "Eve in graph" ("Eve" `elem` names)
  assertBool "Adam in graph" ("Adam" `elem` names)

-- Parse rules test input (already asserted to parse in previous test), then getRelatives and compare to expected.
assertEveRelativesMatch :: Assertion
assertEveRelativesMatch = do
  let parseResult = parseInput rulesGraphEve
  case parseResult of
    Left e -> assertFailure ("rules test input must parse: " ++ errorMessage e)
    Right graph -> do
      case getRelatives rules graph "Eve" of
        Left e -> assertFailure ("getRelatives failed: " ++ e)
        Right vertexToRoles -> do
          -- Convert Map Vertex [String] to Map String [String] for comparison
          let nameToRoles =
                Map.fromListWith (++) [(name v, roles) | (v, roles) <- Map.toList vertexToRoles]
          let norm = Map.map sort
          norm nameToRoles @?= norm expectedMapEve

-- -----------------------------------------------------------------------------
-- Test data
-- -----------------------------------------------------------------------------

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

rulesGraphEve :: String
rulesGraphEve =
  unlines
    [ "Eve (Ж)",
      "Adam (М)",
      "Anna (Ж)",
      "Sonny (М)",
      "Dottie (Ж)",
      "Ed (М)",
      "Abe (М)",
      "Abby (Ж)",
      "Brother (М)",
      "Sister (Ж)",
      "",
      "Eve <-> Ed",
      "Adam <-> Anna",
      "Abe <-> Abby",
      "",
      "Adam -> Eve",
      "Anna -> Eve",
      "Adam -> Brother",
      "Anna -> Brother",
      "Adam -> Sister",
      "Anna -> Sister",
      "Eve -> Sonny",
      "Eve -> Dottie",
      "Abe -> Adam",
      "Abby -> Adam"
    ]

expectedMapEve :: Map.Map String [String]
expectedMapEve =
  Map.fromList
    [ ("Adam", ["отец"]),
      ("Anna", ["мать"]),
      ("Sonny", ["сын"]),
      ("Dottie", ["дочь"]),
      ("Ed", ["муж"]),
      ("Abe", ["дедушка"]),
      ("Abby", ["бабушка"]),
      ("Brother", ["брат"]),
      ("Sister", ["сестра"])
    ]
