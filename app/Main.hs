import Control.Monad (forM_)

import ParseInput
import Types
import Rules

main :: IO ()
main = do
    n <- readFile "test.txt"
    let graph = parseInput n
    let entry = filter (\v -> name v == "A") graph
    forM_ rules $ \(rule, role) -> do
        forM_ (rule entry) $ \v -> do
            putStrLn $ show v ++ " " ++ role
