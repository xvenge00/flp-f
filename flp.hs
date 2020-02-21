import System.Environment
import System.Exit
import System.IO

splitBy delimiter = foldr f [[]] 
    where f c l@(x:xs) | c == delimiter = []:l
                       | otherwise = (c:x):xs

-- read first line
-- states <- split by commas

-- read second line
-- alphabet <- chars are already separated in char[]

-- read third line
-- starts_state <- whole line is state name
-- check that it is contained in list of states

-- read fourth line
-- final_states <- get list separated by commas
-- check that all are contained in 

-- recoursively read line until EOF
-- on each line call split by commas
-- check states in states
-- check char in alphabet
-- rules <- append every rule

-- TODO algoritmus

-- zapis

-- foo <- readFile "test.in"

foo :: String -> String
foo x = x

determinize :: FSA -> FSA
determinize dka = dka

-- data FSA = FSA {
--     states::[String],
--     alphabet::[Char],
--     start_state::String,
--     final_states::[String],
--     rules::[(String, Char, String)]   -- TODO (String, Char, String)
-- }

-- data State = State String
--     deriving (Eq, Show, Read)

type State = String

data FSA = FSA {
    states::[State],
    alphabet::[Char],
    start_state::State,
    final_states::[State]
} deriving (Eq, Show)

-- TODO Read FSA

-- main = do  
--     contents <- readFile "test.in"
--     let content = splitBy '\n' contents
    
--     let rka = FSA{states = splitBy ',' (content !! 0), alphabet = content !! 1, start_state = content !! 2, final_states = splitBy ',' (content !! 3)}
--     -- let rka = (splitBy ',' (content !! 0), content !! 1, content !! 2, splitBy ',' (content !! 3), drop 4 content)

--     let tmp = splitBy ',' (content !! 0)
--     return tmp
--     -- putStrLn contents
--     -- return contents
--     -- putStr contents

-- parse2fsa :: String -> String
-- parse2fsa x = x 

-- -- transform algo
-- rka2dka rka = unlines . reverse . lines rka

-- -- parsing command line arguments
-- parseArgs ["-i"] = getContents >>= return . parse2fsa
-- parseArgs ["-t"] = getContents >>= return . rka2dka . parse2fsa
-- parseArgs ["h"] = usage >> exit
-- parse fs = concat `fmap` mapM readFile fs

-- helper functions
usage   = putStrLn "Usage: rka-2-dka [-ith] [file]"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)

-- -- main
-- main = getArgs >>= parseArgs >>= putStr . show

getFile :: [String] -> IO String
getFile ["-i"] = getContents
getFile ["-t"] = getContents
getFile ["-i",file] = readFile file
getFile ["-t",file] = readFile file
getFile _ = usage >> exit

main = do
    args <- getArgs
    content <- getFile args
    -- if args == "-i":_
    --     then putStrLn "iiiii"
    --     else putStrLn "nee"
    return ()
    
    -- fsa <- parseArgs args
