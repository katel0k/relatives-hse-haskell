import CommonRules (commonRules)
import Data.Char (toLower)
import Data.List (isInfixOf)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import ParseInput
import RuleUtils (a, f, i, m, technicalRuleNames)
import Rules (mergeRules, resolveRulesMap, withoutTechnicalRules, (|:), (<==), RuleExpr)
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
      testGroup "mergeRules" mergeRulesTests,
      testGroup "technical rules not exposed" technicalRulesTests
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
      assertEveRelativesMatch,
    testCase "resolveRulesMap succeeds for commonRules" $
      case resolveRulesMap commonRules of
        Left e -> assertFailure ("expected Right: " ++ errorMessage e)
        Right resolved -> length resolved @?= HashMap.size commonRules,
    testCase "resolveRulesMap fails when a rule references a missing rule" $ do
      let badMap = HashMap.fromList ["bad" |: "nonexistent"]
      case resolveRulesMap badMap of
        Left e -> do
          let msg = errorMessage e
          assertBool "error message mentions missing reference" ("not found" `isInfixOf` msg)
        Right _ -> assertFailure "expected Left for missing rule reference",
    testCase "composite rule: прадедушка = прародитель <== i m (great-grandfather)"
      assertCompositeRulePradedushka,
    testCase "composite rule: прабабушка = прародитель <== i f (great-grandmother)"
      assertCompositeRulePrababushka
  ]

technicalRulesTests :: [TestTree]
technicalRulesTests =
  [ testCase "withoutTechnicalRules removes technical rule names from resolved map" $
      case resolveRulesMap compositeRulesMap of
        Left e -> assertFailure ("resolve failed: " ++ errorMessage e)
        Right resolved -> do
          let filtered = withoutTechnicalRules technicalRuleNames resolved
          length filtered @?= 2
          let names = map snd filtered
          assertBool "прародитель not in filtered" ("прародитель" `notElem` names)
          assertBool "прадедушка in filtered" ("прадедушка" `elem` names)
          assertBool "прабабушка in filtered" ("прабабушка" `elem` names),
    testCase "getRelatives with filtered map never returns technical role прародитель" $ do
      case parseInput inputGraphFourGens of
        Left e -> assertFailure ("parse failed: " ++ errorMessage e)
        Right graph -> do
          case resolveRulesMap compositeRulesMap of
            Left err -> assertFailure ("resolve failed: " ++ errorMessage err)
            Right resolved -> do
              let filtered = withoutTechnicalRules technicalRuleNames resolved
                  eve = head $ filter ((== "Eve") . name) graph
                  pairs = getRelatives filtered eve
                  roles = map snd pairs
              assertBool "прародитель never appears as a role" ("прародитель" `notElem` roles)
              assertBool "прадедушка appears for GreatGrandpa" $
                any (\(v, r) -> name v == "GreatGrandpa" && r == "прадедушка") pairs
              assertBool "прабабушка appears for GreatGrandma" $
                any (\(v, r) -> name v == "GreatGrandma" && r == "прабабушка") pairs
  ]

compositeRulesMap :: HashMap.HashMap String RuleExpr
compositeRulesMap =
  HashMap.fromList
    [  i a <== i a |: "прародитель",
       "прародитель" <== i m |: "прадедушка",
       "прародитель" <== i f |: "прабабушка"
    ]

assertCompositeRulePradedushka :: Assertion
assertCompositeRulePradedushka = do
  case parseInput inputGraphFourGens of
    Left e -> assertFailure ("parse failed: " ++ errorMessage e)
    Right graph -> do
      let eve = head $ filter ((== "Eve") . name) graph
      case resolveRulesMap compositeRulesMap of
        Left err -> assertFailure ("resolve failed: " ++ errorMessage err)
        Right resolved -> do
          let relatives = getRelatives resolved eve
              roleOf n = lookup n [(name v, role) | (v, role) <- relatives]
          assertBool "Eve's great-grandfather (GreatGrandpa) has role прадедушка" $
            roleOf "GreatGrandpa" == Just "прадедушка"
          assertBool "Eve's great-grandmother (GreatGrandma) has role прабабушка" $
            roleOf "GreatGrandma" == Just "прабабушка"

assertCompositeRulePrababushka :: Assertion
assertCompositeRulePrababushka = do
  case parseInput inputGraphFourGens of
    Left e -> assertFailure ("parse failed: " ++ errorMessage e)
    Right graph -> do
      let eve = head $ filter ((== "Eve") . name) graph
      case resolveRulesMap compositeRulesMap of
        Left err -> assertFailure ("resolve failed: " ++ errorMessage err)
        Right resolved -> do
          let relatives = getRelatives resolved eve
              roleOf n = lookup n [(name v, role) | (v, role) <- relatives]
          roleOf "GreatGrandma" @?= Just "прабабушка"

mergeRulesTests :: [TestTree]
mergeRulesTests =
  [ testCase "mergeRules empty bs has same size as bs" $
      HashMap.size (mergeRules HashMap.empty commonRules) @?= HashMap.size commonRules,
    testCase "mergeRules as empty has same size as as" $
      HashMap.size (mergeRules commonRules HashMap.empty) @?= HashMap.size commonRules,
    testCase "mergeRules as bs has size size as + size bs when disjoint" $ do
      let entries = HashMap.toList commonRules
          as = HashMap.fromList (take 5 entries)
          bs = HashMap.fromList (drop 5 entries)
      HashMap.size (mergeRules as bs) @?= HashMap.size as + HashMap.size bs,
    testCase "mergeRules: result contains all keys from both maps" $ do
      let as = HashMap.fromList (take 2 (HashMap.toList commonRules))
          bs = HashMap.fromList (take 2 (drop 2 (HashMap.toList commonRules)))
          merged = mergeRules as bs
      HashMap.size merged @?= 4
      assertBool "keys from first map present" $
        all (`HashMap.member` merged) (HashMap.keys as)
      assertBool "keys from second map present" $
        all (`HashMap.member` merged) (HashMap.keys bs)
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
      case resolveRulesMap commonRules of
        Left e -> assertFailure ("rule resolution failed: " ++ errorMessage e)
        Right resolved -> do
          let pairs = getRelatives resolved eve
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

-- | Four generations: GreatGrandpa -> Abe -> Adam -> Eve, GreatGrandma -> Abby -> Adam -> Eve.
-- From Eve, прадедушка = GreatGrandpa, прабабушка = GreatGrandma.
inputGraphFourGens :: String
inputGraphFourGens =
  unlines
    [ "Eve (Ж)",
      "Adam (М)",
      "Anna (Ж)",
      "Abe (М)",
      "Abby (Ж)",
      "GreatGrandpa (М)",
      "GreatGrandma (Ж)",
      "",
      "Adam <-> Anna",
      "Abe <-> Abby",
      "",
      "GreatGrandpa -> Abe",
      "GreatGrandma -> Abby",
      "Abe -> Adam",
      "Abby -> Adam",
      "Adam -> Eve",
      "Anna -> Eve"
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
