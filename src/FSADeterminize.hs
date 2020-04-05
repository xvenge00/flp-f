module FSADeterminize (determinize) where

import Data.List (union, (\\), sort, nub, intersect)
import qualified Data.Map.Strict as Map (fromList, findWithDefault)
-- import qualified Data.Set as Set

import FSATypes

-- TODO zoznamy stavov do setov

-- Function transforms an extended finate state automata to a deterministic finate state automaton
-- old    <- a valid extended FSA
-- return <- valid deterministic FSA  
determinize :: FSA -> FSA
determinize old =
    FSA (map rename set_states)
        (sort $ nub [c_s rule | rule <- set_rules])
        (rename startEClosure)
        (nub $ [rename s | s <- set_states, not $ null $ final_states old `intersect` s])
        [Rule (rename $ from_s r) (c_s r) (rename $ to_s r) | r <- set_rules]
            where
                set_rules = sort $ makeNewRules (rules old) (alphabet old) (start_state old)
                set_states = nub $ startEClosure : map to_s set_rules
                startEClosure = eClosure [start_state old] (rules old)
                rename set = Map.findWithDefault (-1) set $ Map.fromList $ zip set_states [0..]


-- Get states reachable from `state` by epsilon in one step based on rules `r`
statesThroughEpsilon :: Rules -> State -> States
statesThroughEpsilon r state = nub [ n | EpsilonRule s n <- r, s == state]


-- Create epsilon closure from list of states `s` based on rules `r`
eClosure :: States -> Rules -> States
eClosure s r =
    sort $ eClosure' s r []
        where
            eClosure' :: States -> Rules -> States -> States
            eClosure' [] _ explored = sort explored
            eClosure' unexplored@(s:_) rules explored =
                eClosure' new_unexplored rules new_expored
                    where 
                        new_expored = s:explored
                        new_unexplored =
                            unexplored `union` statesThroughEpsilon rules s \\ new_expored


-- from which `states` using `by` symbol based on `rules`
-- generaly should have epsilon closure on input
reachableBy :: States -> Char -> Rules -> States
reachableBy states by rules =
    sort $ nub $ concat
        [ eClosure [next] rules | Rule curr c next <- rules, c == by, curr `elem` states]


-- TODO change Rule to be more general
data SetRule = SetRule {from_s::States, c_s::Symbol, to_s::States}
    deriving (Eq, Show, Ord)
type SetRules = [SetRule]

-- creates table with set of States representing rules
makeNewRules :: Rules -> Alphabet -> State -> SetRules
makeNewRules rules alphabet start =
    makeNewRules' rules alphabet [eClosure [start] rules] []

-- implementation of makeNewRules
makeNewRules' :: Rules -> Alphabet -> [States] -> SetRules -> SetRules
makeNewRules' _ _ [] rules = rules
makeNewRules' rules alphabet [exploring] set_rules =
    makeNewRules' rules alphabet unexplored (new_rules ++ set_rules)
        where
            unexplored = [x | x <- generated, x `notElem` explored && (x /= [])]
            explored = map from_s (new_rules ++ set_rules)
            new_rules = [SetRule exploring by to | (by, to) <- zip alphabet generated, not $ null to]
            generated = map (\c -> reachableBy exploring c rules) alphabet 
makeNewRules' rules alphabet (exploring:unexplored) new_rules = 
    makeNewRules' rules alphabet (unexplored \\ explored) table_fst
        where 
            table_fst = makeNewRules' rules alphabet [exploring] new_rules
            explored = map from_s table_fst

