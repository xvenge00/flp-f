module FSATypes where

import Data.List (intercalate)

-- =========== TYPES ====================
-- State
type State = Integer
type States = [State]

type Alphabet = [Char]

data Rule = Rule {current::State, c::Char, next::State} | EpsilonRule State State
    deriving (Eq, Show)
type Rules = [Rule]

data FSA = FSA {
    states::States,
    alphabet::Alphabet,
    start_state::State,
    final_states::States,
    rules::Rules
} deriving (Eq, Show)

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
