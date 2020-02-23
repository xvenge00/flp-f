import System.Environment
import System.Exit
import System.IO
import Data.Maybe

-- utils
splitBy delimiter = foldr f [[]] 
    where f c l@(x:xs) | c == delimiter = []:l
                       | otherwise = (c:x):xs


removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys

-- type definitions
type State = String

data Rule = Rule State Char State
    deriving (Eq, Show)

type Alphabet = [Char]

parseRule :: [String] -> Maybe Rule
parseRule [state, [char], stateNext] = Just $ Rule state char stateNext
parseRule [state, [], stateNext] = Just $ Rule state '-' stateNext
parseRule _ = Nothing

parseRulesImpl :: [String] -> [Maybe Rule]
parseRulesImpl lines
    |lines == [] = []
    |otherwise = parseRule ( splitBy ',' $ head lines ) : (parseRulesImpl $ tail lines)

parseRules :: [String] -> Maybe [Rule]
parseRules lines = sequence $ parseRulesImpl lines
    
parseState :: String -> State
parseState state = state

parseStates :: String -> [State]
parseStates states = map parseState (splitBy ',' states)

parseAlphabet :: String -> Alphabet
parseAlphabet alphabet = alphabet

data FSA = FSA {
    states::[State],
    alphabet::Alphabet,
    start_state::State,
    final_states::[State],
    rules::[Rule]
} deriving (Eq, Show)


-- TODO dopln kontrolu na to aby to bolo konzistentne
parse2FSA :: String -> Maybe FSA
parse2FSA repr = do
    let lines = splitBy '\n' repr
    let states = parseStates (lines !! 0)
    let alphabet = parseAlphabet (lines !! 1)
    let start_state = parseState (lines !! 2)
    let final_states = parseStates (lines !! 3)
    rules <- parseRules (removeItem "" $ drop 4 lines)  -- TODO maybe remove only last element?
    Just $ FSA states alphabet start_state final_states rules


determinize :: FSA -> FSA
-- TODO
determinize dka = FSA ["staaaaaav1", "s2"] "abc" "s1" ["s1","s2"] [Rule "a" 'a' "a"]


-- helper functions
usage = putStrLn "Usage: rka-2-dka [-ith] [file]"
exit = exitWith ExitSuccess
exitFail = exitWith (ExitFailure 1)
wrongFormat = putStrLn "Wrong format."

-- parsing command line arguments
getFile :: [String] -> IO String
getFile ["-i"] = getContents
getFile ["-t"] = getContents
getFile ["-i",file] = readFile file
getFile ["-t",file] = readFile file
getFile _ = usage >> exit

main = do
    args <- getArgs
    content <- getFile args
    case parse2FSA content of 
        Just x -> putStrLn $ show $ if head args == "-t"
                                        then determinize x 
                                        else x
        Nothing -> wrongFormat >> exitFail
