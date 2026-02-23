import CommonRules (commonRules)
import Data.Char (toLower)
import qualified Data.Map.Strict as Map
import ParseInput
import Rules (mergeRules)
import Run
import Test.Tasty
import Test.Tasty.HUnit
import Types (Gender (..), Graph, errorMessage, gender, name)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "All"
    [ testGroup "parsing input" parsingTests,
      testGroup "rules evaluation" rulesTests,
      testGroup "mergeRules" mergeRulesTests
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
      case parseInput "Alice basically_any_gibberish\n\n\n" of
        Left _ -> pure ()
        Right _ -> assertFailure "expected Left for invalid gender",
    testCase "duplicate name in gender section gives Left" $
      case parseInput "Alice (Ж)\nBob (М)\nAlice (Ж)\n\n\n" of
        Left e -> do
          let msg = errorMessage e
          assertBool "error mentions duplicate" ("duplicate" `elem` words (map toLower msg))
          assertBool "error mentions Alice" ("Alice" `elem` words msg)
        Right _ -> assertFailure "expected Left for duplicate name",
    testCase "duplicate parent-child edge gives Left" $
      case parseInput "A (М)\nB (Ж)\n\nA <-> B\n\nA -> B\nA -> B\n" of
        Left e -> do
          let msg = errorMessage e
          assertBool "error mentions duplicate" ("duplicate" `elem` words (map toLower msg))
        Right _ -> assertFailure "expected Left for duplicate parent-child edge",
    testCase "duplicate marriage edge (same pair) gives Left" $
      case parseInput "X (М)\nY (Ж)\n\nX <-> Y\nX <-> Y\n\n\n" of
        Left e -> do
          let msg = errorMessage e
          assertBool "error mentions duplicate" ("duplicate" `elem` words (map toLower msg))
        Right _ -> assertFailure "expected Left for duplicate marriage edge",
    testCase "duplicate marriage edge (reversed pair) gives Left" $
      case parseInput "X (М)\nY (Ж)\n\nX <-> Y\nY <-> X\n\n\n" of
        Left e -> do
          let msg = errorMessage e
          assertBool "error mentions duplicate" ("duplicate" `elem` words (map toLower msg))
        Right _ -> assertFailure "expected Left for duplicate marriage edge (reversed)"
  ]

rulesTests :: [TestTree]
rulesTests =
  [ testCase "rules test input parses to expected graph" $
      assertParsedRulesGraph inputGraphEve expectedRulesGraphShape,
    testCase
      "Eve's relatives match expected name->roles map (commonRules, at most two-deep)"
      assertEveRelativesMatch
  ]

mergeRulesTests :: [TestTree]
mergeRulesTests =
  [ testCase "mergeRules [] bs has same length as bs" $
      length (mergeRules [] commonRules) @?= length commonRules,
    testCase "mergeRules as [] has same length as as" $
      length (mergeRules commonRules []) @?= length commonRules,
    testCase "mergeRules as bs has length length as + length bs" $ do
      let as = take 5 commonRules
          bs = drop 5 commonRules
      length (mergeRules as bs) @?= length as + length bs,
    testCase "mergeRules preserves order: first list then second (by role labels)" $ do
      let as = take 2 commonRules
          bs = take 2 $ drop 2 commonRules
          merged = mergeRules as bs
      length merged @?= 4
      map snd (take 2 merged) @?= map snd as
      map snd (drop 2 merged) @?= map snd bs
  ]

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

assertEveRelativesMatch :: Assertion
assertEveRelativesMatch = do
  let parseResult = parseInput inputGraphEve
  case parseResult of
    Left e -> assertFailure ("rules test input must parse: " ++ errorMessage e)
    Right graph -> do
      let eve = head $ filter ((== "Eve") . name) graph
          pairs = getRelatives commonRules eve
          nameToRoles = Map.fromList [(name v, role) | (v, role) <- pairs]
      nameToRoles @?= expectedMapEve

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

inputGraphEve :: String
inputGraphEve =
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

expectedMapEve :: Map.Map String String
expectedMapEve =
  Map.fromList
    [ ("Adam", "отец"),
      ("Anna", "мать"),
      ("Sonny", "сын"),
      ("Dottie", "дочь"),
      ("Ed", "муж"),
      ("Abe", "дедушка"),
      ("Abby", "бабушка"),
      ("Brother", "брат"),
      ("Sister", "сестра")
    ]
