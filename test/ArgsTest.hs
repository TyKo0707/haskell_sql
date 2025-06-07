module ArgsTest (runArgTests) where

import TestFramework

data Mode
    = AutoMode FilePath String FilePath
    | InteractiveMode FilePath
    | InvalidArgs
    deriving (Eq, Show)

parseArgs :: [String] -> Mode
parseArgs (a:b:c:_) = AutoMode a b c
parseArgs [a]       = InteractiveMode a
parseArgs _         = InvalidArgs

runArgTests :: IO ()
runArgTests = do
    putStrLn "\nRunning ArgsTest..."
    assertEqual "AutoMode with 3 args"
        (AutoMode "file.csv" "SELECT *" "out.csv")
        (parseArgs ["file.csv", "SELECT *", "out.csv"])

    assertEqual "InteractiveMode with 1 arg"
        (InteractiveMode "data.csv")
        (parseArgs ["data.csv"])

    assertEqual "InvalidArgs with no input"
        InvalidArgs
        (parseArgs [])

    assertEqual "InvalidArgs with 2 args"
        InvalidArgs
        (parseArgs ["only.csv", "query"])
