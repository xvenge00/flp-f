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
newtype State = State String
    deriving (Eq)
instance Show State where
    show (State s) = s
    -- showList rs = (show rs) (++)

newtype States = States [State]
    deriving (Eq)
instance Show States where
    show (States []) = ""
    show (States [s]) = show s
    show (States ls) = show (head ls) ++ "," ++ show (States (tail ls))

data Rule = Rule State Char State
    deriving (Eq, Show)

type Alphabet = [Char]

parseRule :: [String] -> Maybe Rule
parseRule [state, [char], stateNext] = Just $ Rule (parseState state) char (parseState stateNext)
parseRule [state, [], stateNext] = Just $ Rule (parseState state) '-' (parseState stateNext)
parseRule _ = Nothing

parseRulesImpl :: [String] -> [Maybe Rule]
parseRulesImpl lines
    |lines == [] = []
    |otherwise = parseRule ( splitBy ',' $ head lines ) : (parseRulesImpl $ tail lines)

parseRules :: [String] -> Maybe [Rule]
parseRules lines = sequence $ parseRulesImpl lines
    
parseState :: String -> State
parseState state = State state

parseStates :: String -> States
parseStates states = States $ map parseState (splitBy ',' states)

parseAlphabet :: String -> Alphabet
parseAlphabet alphabet = alphabet

data FSA = FSA {
    states::States,
    alphabet::Alphabet,
    start_state::State,
    final_states::States,
    rules::[Rule]
} deriving (Eq)

-- instance Show State where
--   show (String x) = show x

--   showList rs = unlines (map show rs)

instance Show FSA where
  show (FSA state alphabet start_state final_states rules) = show state ++ "\n" ++ show alphabet ++ "\n" ++ show start_state ++ "\n" ++ show final_states ++ "\n" ++ show rules


-- TODO dopln kontrolu na to aby to bolo konzistentne
parse2FSA :: String -> Maybe FSA
parse2FSA repr = do
    let lines = splitBy '\n' repr
        states = parseStates (lines !! 0)
        alphabet = parseAlphabet (lines !! 1)
        start_state = parseState (lines !! 2)
        final_states = parseStates (lines !! 3)
    rules <- parseRules (removeItem "" $ drop 4 lines)  -- TODO maybe remove only last element?
    Just $ FSA states alphabet start_state final_states rules


determinize :: FSA -> FSA
-- TODO
determinize dka = FSA (States [State "staaaaaav1", State "s2"]) "abc" (State "s1") (States [State "s1", State "s2"]) [Rule (State "a") 'a' (State "a")]


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
