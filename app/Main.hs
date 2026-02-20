import Control.Monad (forM_)
import ParseInput
import Rules
import Types
import qualified Data.HashSet as HS
import Control.Monad.State (evalState)

main :: IO ()
main = do
  n <- readFile "test.txt"
  let graph = parseInput n
  let entry = filter (\v -> name v == "A") graph
  forM_ rules $ \(rule, role) -> do
    let result = evalState (rule entry) HS.empty
    forM_ result $ \v -> do
      putStrLn $ show v ++ " " ++ role
