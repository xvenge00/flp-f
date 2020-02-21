import System.Environment
import System.Exit
import System.IO

splitBy delimiter = foldr f [[]] 
    where f c l@(x:xs) | c == delimiter = []:l
                       | otherwise = (c:x):xs

type State = String

data FSA = FSA {
    states::[State],
    alphabet::[Char],
    start_state::State,
    final_states::[State]
} deriving (Eq, Show)

-- TODO
-- data FSA = FSA {
--     states::[State],
--     alphabet::[Char],
--     start_state::State,
--     final_states::[State],
--     rules::[(State, Char, State)]
-- } deriving (Eq, Show)

parse2FSA :: String -> FSA
-- TODO dopln pravidla
-- TODO dopln kontrolu na to aby to bolo konzistentne
parse2FSA repr = 
    let lines = splitBy '\n' repr
    in FSA{states = splitBy ',' (lines !! 0), alphabet = lines !! 1, start_state = lines !! 2, final_states = splitBy ',' (lines !! 3)}

determinize :: FSA -> FSA
-- TODO
determinize dka = FSA ["staaaaaav1", "s2"] "abc" "s1" ["s1","s2"]


-- helper functions
usage   = putStrLn "Usage: rka-2-dka [-ith] [file]"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)

-- parsing command line arguments
getFile :: [String] -> IO String
getFile ["-i"] = getContents
getFile ["-t"] = getContents
getFile ["-i",file] = readFile file
getFile ["-t",file] = readFile file
getFile _ = usage >> exit

-- main
main = do
    args <- getArgs
    content <- getFile args
    let fsa = if head args == "-t"
                then (determinize . parse2FSA) content
                else parse2FSA content
    putStrLn $ show fsa
