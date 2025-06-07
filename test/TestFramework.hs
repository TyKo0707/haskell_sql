module TestFramework (assertEqual, assertFailure) where

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual =
    if expected == actual
        then putStrLn $ "[PASS] " ++ label
        else do
            putStrLn $ "[FAIL] " ++ label
            putStrLn $ "  Expected: " ++ show expected
            putStrLn $ "  But got:  " ++ show actual

assertFailure :: String -> IO ()
assertFailure msg = putStrLn $ "[FAIL] " ++ msg
