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
    show (States s) = tail $ foldl addComma "" (map show s)
                            where addComma a b = a ++ "," ++ b

newtype AlphabetChar = AlphabetChar Char
    deriving (Eq)
instance Show AlphabetChar where
    show (AlphabetChar c) = [c]

newtype Alphabet = Alphabet [AlphabetChar]
    deriving (Eq)
instance Show Alphabet where
    show (Alphabet a) = foldl (++) [] (map show a)

data Rule = Rule State AlphabetChar State
    deriving Eq
instance Show Rule where
    show (Rule state char state_next) = show state ++ "," ++ show char ++ "," ++ show state_next    -- TODO don't show epsilon (-)

data Rules = Rules [Rule]
    deriving Eq
instance Show Rules where
    show (Rules []) = ""
    show (Rules r) = tail $ foldl addNewLine "" (map show r)
                            where addNewLine a b = a ++ "\n" ++ b


parseRule :: [String] -> Maybe Rule
parseRule [state, [char], stateNext] = Just $ Rule (parseState state) (AlphabetChar char) (parseState stateNext)
parseRule [state, [], stateNext] = Just $ Rule (parseState state) (AlphabetChar '-') (parseState stateNext)
parseRule _ = Nothing

parseRulesImpl :: [String] -> [Maybe Rule]
parseRulesImpl lines
    |lines == [] = []
    |otherwise = parseRule ( splitBy ',' $ head lines ) : (parseRulesImpl $ tail lines)

parseRules :: [String] -> Maybe Rules
parseRules lines = case sequence $ parseRulesImpl lines of
                    Just x -> Just $ Rules x
                    Nothing -> Nothing
    
parseState :: String -> State
parseState state = State state

parseStates :: String -> States
parseStates states = States $ map parseState (splitBy ',' states)

parseAlphabet :: String -> Alphabet
parseAlphabet alphabet = Alphabet $ map AlphabetChar alphabet

data FSA = FSA {
    states::States,
    alphabet::Alphabet,
    start_state::State,
    final_states::States,
    rules::Rules
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
determinize dka = FSA (States [State "staaaaaav1", State "s2"]) (Alphabet [AlphabetChar 'a', AlphabetChar 'b', AlphabetChar 'c']) (State "s1") (States [State "s1", State "s2"]) (Rules [Rule (State "a") (AlphabetChar 'a') (State "a")])


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
