{-|
  rka-2-dka
  author: Adam Venger (xvenge00)
  year: 2020
  
  Functional project FLP
-}

module FSATypes where

import Data.List (intercalate)

-- =========== TYPES ====================
-- Representation of state
type State = Integer
type States = [State]

-- representation of symbols in FSA
type Symbol = Char
type Alphabet = [Symbol]

-- representation of Rules in FSA
data Rule = EpsilonRule {current::State, next::State} |
            Rule {current::State, c::Symbol, next::State}
    deriving (Eq, Show, Ord)
type Rules = [Rule]

-- representation of FSA
data FSA = FSA {
    states::States,
    alphabet::Alphabet,
    start_state::State,
    final_states::States,
    rules::Rules
} deriving (Eq, Show)

-- -- ============= 2 STRING =================

-- convert states to string required format
showState :: State -> String
showState = show
showStates :: States -> String
showStates states = intercalate "," $ map showState states

-- convert rule to string required format
showRule :: Rule -> String
showRule (Rule state char state_next) =
    showState state ++ "," ++ [char] ++ "," ++ showState state_next ++ "\n"
showRule (EpsilonRule state state_next) =
    showState state ++ ",," ++ showState state_next ++ "\n"
showRules :: Rules -> String
showRules = concatMap showRule

-- convert fsa to string required format
showFSA :: FSA -> String
showFSA (FSA states alphabet start_state final_states rules) =
    intercalate "\n" [showStates states,
                      alphabet,
                      showState start_state,
                      showStates final_states,
                      showRules rules]
