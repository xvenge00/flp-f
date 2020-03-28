-- FLP Funkcionálny projekt rka-2-dka
-- Adam Venger (xvenge00)
-- 2020

import System.Environment
import System.Exit
import System.IO
import Data.List (union, delete, (\\), sort, nub, intersect, intercalate, mapAccumL)
import qualified Data.Map.Strict as Map (fromList, findWithDefault)

-- TODO hlint

-- =========== UTILS ======================
splitBy delimiter = foldr f [[]] 
    where f c l@(x:xs) | c == delimiter = []:l
                       | otherwise = (c:x):xs

-- =========== TYPES ====================
-- State
type State = Integer

parseState :: String -> State
parseState state = read state :: Integer
type States = [State]

parseStates :: String -> States
parseStates states = map parseState (splitBy ',' states)

type Alphabet = [Char]

parseAlphabet :: String -> Alphabet
parseAlphabet alphabet = alphabet


-- Rule
data Rule = Rule {current::State, c::Char, next::State} | EpsilonRule State State
    deriving (Eq, Show)

parseRule :: [String] -> Maybe Rule
parseRule [state, [char], stateNext] = Just $ Rule (parseState state) char (parseState stateNext)
parseRule [state, [], stateNext] = Just $ EpsilonRule (parseState state) (parseState stateNext)
parseRule _ = Nothing

type Rules = [Rule]

-- TODO move implementation into parseRules
parseRulesImpl :: [String] -> [Maybe Rule]
parseRulesImpl lines
    |lines == [] = []
    |otherwise = parseRule ( splitBy ',' $ head lines ) : (parseRulesImpl $ tail lines)

parseRules :: [String] -> Maybe Rules
parseRules lines = case sequence $ parseRulesImpl lines of
                    Just x -> Just $ x
                    Nothing -> Nothing
    

-- FSA
data FSA = FSA {
    states::States,
    alphabet::Alphabet,
    start_state::State,
    final_states::States,
    rules::Rules
} deriving (Eq, Show)

-- TODO dopln kontrolu na to aby to bolo konzistentne
valid :: FSA -> Bool
valid fsa = True

parse2FSA :: String -> Maybe FSA
parse2FSA repr = do
    let dfa_lines = lines repr  -- TODO nemusi tam byt index po 4!!!!!!!!!!!!!!!!!!!!!!!!
        states = parseStates (dfa_lines !! 0)
        alphabet = parseAlphabet (dfa_lines !! 1)
        start_state = parseState (dfa_lines !! 2)
        final_states = parseStates (dfa_lines !! 3)
    rules <- parseRules (delete "" $ drop 4 dfa_lines)  -- TODO maybe remove only last element?
    let fsa = FSA states alphabet start_state final_states rules
    if valid fsa
        then Just fsa
        else Nothing


-- -- ============= 2 STRING =================

-- TODO custom interface??
state2str :: State -> String
state2str s = show s
states2str :: States -> String
states2str states = intercalate "," $ map state2str states

showRule :: Rule -> String
showRule (Rule state char state_next) = state2str state ++ "," ++ [char] ++ "," ++ state2str state_next
showRule (EpsilonRule state state_next) = state2str state ++ ",," ++ state2str state_next
showRules :: Rules -> String
showRules r = intercalate "\n" $ map showRule r

showFSA :: FSA -> String
showFSA (FSA states alphabet start_state final_states rules) = intercalate "\n" [states2str states, alphabet, state2str start_state, states2str final_states, showRules rules]

-- ========================== ALGORITHM =======================
determinize :: FSA -> FSA
determinize old =
    let rule_table_sets = makeRuleTableWithSets (rules old) (alphabet old) (start_state old)
        rename_map = makeRenameMap rule_table_sets
        rule_table_renamed = renameTable rename_map rule_table_sets
        new_rules = makeRules rule_table_renamed (alphabet old)
        new_states = map (fst) rule_table_renamed
        new_alphabet = foldl (\acc r -> union acc [c r]) "" new_rules
        new_final = newFinalStates (final_states old) (map (fst) rule_table_sets) rename_map
        new_start = Map.findWithDefault 0 (eClosure [start_state old] (rules old)) rename_map
    in  FSA new_states new_alphabet new_start new_final new_rules

statesThroughEpsilon :: Rules -> State -> States
statesThroughEpsilon [] _ = []
statesThroughEpsilon [Rule _ _ _] _ = []
statesThroughEpsilon [EpsilonRule s n] state =
    if s == state
        then [n]
        else []
statesThroughEpsilon (r:rs) state = union (statesThroughEpsilon [r] state) (statesThroughEpsilon rs state)

-- ktore preskumat, pravidla -> vysledny  uzaver
eClosure :: States -> Rules -> States
eClosure s r =
    eClosure' s r []
        where
            -- ktore preskumat, pravidla, preskumane stavy -> vysledny  uzaver
            eClosure' :: States -> Rules -> States -> States
            eClosure' [] _ explored = sort explored
            eClosure' unexplored rules explored =
                eClosure' new_unexplored rules new_expored
                    where s = head unexplored
                          new_expored = s:explored
                          new_unexplored = (union unexplored (statesThroughEpsilon rules s)) \\ new_expored


-- z ktorych stavov, akym znakom, pravidla -> vysledok
-- should have epsilon closure on input
reachableIn1 :: States -> Char -> Rules -> States
reachableIn1 [] _ _ = []
reachableIn1 (s:ss) c rules =
    sort $ union (reachableBy c rules) (reachableIn1 ss c rules)
        where reachableBy :: Char -> Rules -> States
              reachableBy _ [] = []
              reachableBy char [Rule i_s c n] =
                  if char == c && i_s == s
                      then eClosure [n] rules
                      else []
              reachableBy _ [EpsilonRule _ _] = []
              reachableBy c (r:rs) = union (reachableBy c [r]) (reachableBy c rs)


-- type RuleTableRow = (States, [States])
type RuleTable a = [(a, [a])]
type RuleTableWithSets = RuleTable States
type RuleTableSimple = RuleTable State

makeRuleTableWithSets rules alphabet start = makeRuleTable' rules alphabet [eClosure [start] rules] []

-- alphabet should be sorted
makeRuleTable' :: Rules -> [Char] -> [States] -> RuleTableWithSets -> RuleTableWithSets
makeRuleTable' _ _ [] table = table
makeRuleTable' rules alphabet [unexplored] table =
    let now_generated_states = map (\c -> reachableIn1 unexplored c rules) alphabet
        new_row = (unexplored, now_generated_states)
        new_table = table ++ [new_row]
        explored = map (fst) new_table
        new_unexplored = [x | x <- now_generated_states, (notElem x explored) && (x /= [])]
    
    -- all states are explored and no new unexplored states were generated
    in  if null new_unexplored
            then new_table
            else makeRuleTable' rules alphabet new_unexplored new_table
makeRuleTable' rules alphabet (u_fst:u_rest) table = 
    let t_after_fst = makeRuleTable' rules alphabet [sort u_fst] table
    in  nub $ makeRuleTable' rules alphabet u_rest t_after_fst  -- TODO find why nub needed

makeRenameMap table = Map.fromList $ zip states [0..]
                        where states = map (fst) table

renameTable r_map table = map (\row -> (rename (fst row), map (\col -> rename col) (snd row))) table
    where rename state = Map.findWithDefault (-1) state r_map

makeRules :: RuleTableSimple -> Alphabet -> Rules
makeRules table alphabet = foldl (\acc row -> acc ++ (makeRuleFromRow row)) [] table
    where   makeRuleFromRow :: (State, States) -> Rules
            makeRuleFromRow row = foldl (\acc s -> acc ++ (rule (fst row) (fst s) (snd s))) [] (zip alphabet (snd row))
            rule :: State -> Char -> State -> Rules
            rule from c to = if to < 0 then [] else [Rule from c to]

newFinalStates old new m =
    foldl (\acc n -> acc ++ if null $ intersect old n then [] else [Map.findWithDefault (-1) n m]) [] new

-- ====================== PARSRING INPUT ======================
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


-- =========================== MAIN ============================
main = do
    args <- getArgs
    content <- getFile args
    case parse2FSA content of 
        Just x -> putStrLn $ showFSA $ if head args == "-t"
                                        then determinize x 
                                        else x
        Nothing -> wrongFormat >> exitFail

