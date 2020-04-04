module FSADeterminize (determinize) where

import Data.List (union, (\\), sort, nub, intersect)
import qualified Data.Map.Strict as Map (fromList, findWithDefault)

import FSATypes

-- TODO use sets

-- Function transforms an extended finate state automata to a deterministic finate state automaton
-- old    <- a valid extended FSA
-- return <- valid deterministic FSA  
determinize :: FSA -> FSA
determinize old =
    FSA new_states new_alphabet new_start new_final new_rules
        where
            new_states = map fst rule_table_renamed
            new_alphabet = sort $ nub [c rule | rule <- new_rules]
            new_start = Map.findWithDefault 0 (eClosure [start_state old] (rules old)) rename_map
            new_final = newFinalStates (final_states old) (map fst rule_table_sets) rename_map
            new_rules = makeRules rule_table_renamed (alphabet old)
            
            rule_table_sets = makeRuleTable (rules old) (alphabet old) (start_state old)
            rename_map = makeRenameMap rule_table_sets
            rule_table_renamed = renameTable rename_map rule_table_sets
            newFinalStates old new m =
                [Map.findWithDefault (- 1) n m | n <- new, not (null $ intersect old n)]


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
            eClosure' unexplored rules explored =
                eClosure' new_unexplored rules new_expored
                    where s = head unexplored
                          new_expored = s:explored
                          new_unexplored =
                              unexplored `union` statesThroughEpsilon rules s \\ new_expored


-- from which `states` using `by` symbol based on `rules`
-- generaly should have epsilon closure on input
reachableBy :: States -> Char -> Rules -> States
reachableBy states by rules =
    sort $ nub $ concat
        [ eClosure [next] rules | Rule curr c next <- rules, c == by, curr `elem` states]


-- type RuleTableRow = (States, [States])
type RuleTable a = [(a, [a])]
type RuleTableWithSets = RuleTable States
type RuleTableSimple = RuleTable State

makeRuleTable :: Rules -> Alphabet -> State -> RuleTableWithSets
makeRuleTable rules alphabet start = makeRuleTable' rules (sort alphabet) [eClosure [start] rules] []

-- alphabet should be sorted
makeRuleTable' :: Rules -> Alphabet -> [States] -> RuleTableWithSets -> RuleTableWithSets
makeRuleTable' _ _ [] table = table
makeRuleTable' rules alphabet [exploring] table =
    makeRuleTable' rules alphabet unexplored (table ++ [new_row])
        where
            unexplored = [x | x <- generated, x `notElem` explored && (x /= [])]
            explored = map fst (table ++ [new_row])
            new_row = (exploring, generated)
            generated = map (\c -> reachableBy exploring c rules) alphabet 
makeRuleTable' rules alphabet (exploring:unexplored) table = 
    makeRuleTable' rules alphabet (unexplored \\ explored) table_fst
        where 
            table_fst = makeRuleTable' rules alphabet [exploring] table
            explored = map fst table_fst

makeRenameMap table = Map.fromList $ zip states [0..]
                        where states = map fst table

renameTable r_map =
    map (\row -> (rename (fst row), map rename (snd row)))
        where rename state = Map.findWithDefault (-1) state r_map

makeRules :: RuleTableSimple -> Alphabet -> Rules
-- makeRules table alphabet = [makeRuleFromRow row | row <- table]
makeRules table alphabet = foldl (\acc row -> acc ++ makeRuleFromRow row) [] table
    where   makeRuleFromRow :: (State, States) -> Rules
            makeRuleFromRow row =
                foldl (\acc s -> acc ++ uncurry (rule (fst row)) s) [] (zip alphabet (snd row))
            rule :: State -> Char -> State -> Rules
            rule from c to = [Rule from c to | to >= 0]

