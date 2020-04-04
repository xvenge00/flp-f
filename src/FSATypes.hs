module FSATypes where

import Data.List (intercalate)

-- =========== TYPES ====================
-- State
type State = Integer
type States = [State]

type Symbol = Char
type Alphabet = [Symbol]

data Rule = Rule {current::State, c::Symbol, next::State} |
            EpsilonRule {current::State, next::State}
    deriving (Eq, Show, Ord)
type Rules = [Rule]

data FSA = FSA {
    states::States,
    alphabet::Alphabet,
    start_state::State,
    final_states::States,
    rules::Rules
} deriving (Eq, Show)

-- -- ============= 2 STRING =================

showState :: State -> String
showState = show
showStates :: States -> String
showStates states = intercalate "," $ map showState states

showRule :: Rule -> String
showRule (Rule state char state_next) =
    showState state ++ "," ++ [char] ++ "," ++ showState state_next ++ "\n"
showRule (EpsilonRule state state_next) =
    showState state ++ ",," ++ showState state_next ++ "\n"
showRules :: Rules -> String
showRules = concatMap showRule

showFSA :: FSA -> String
showFSA (FSA states alphabet start_state final_states rules) =
    intercalate "\n" [showStates states,
                      alphabet,
                      showState start_state,
                      showStates final_states,
                      showRules rules]
