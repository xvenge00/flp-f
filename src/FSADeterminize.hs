module FSADeterminize (determinize) where

import Data.List (union, (\\), sort, nub, intersect)
import qualified Data.Map.Strict as Map (fromList, findWithDefault)

import FSATypes

-- Function transforms an extended finate state automaton to a deterministic finate state automaton.
-- old    <- a valid extended FSA
-- return <- valid deterministic FSA  
determinize :: FSA -> FSA
determinize old =
    let rule_table_sets = makeRuleTableWithSets (rules old) (alphabet old) (start_state old)
        rename_map = makeRenameMap rule_table_sets
        rule_table_renamed = renameTable rename_map rule_table_sets
        new_rules = makeRules rule_table_renamed (alphabet old)
        new_states = map fst rule_table_renamed
        new_alphabet = sort $ nub [c rule | rule <- new_rules]
        new_final = newFinalStates (final_states old) (map fst rule_table_sets) rename_map
        new_start = Map.findWithDefault 0 (eClosure [start_state old] (rules old)) rename_map
    in  FSA new_states new_alphabet new_start new_final new_rules


-- Get states reachable from `state` by epsilon in one step based on rules `r`
statesThroughEpsilon :: Rules -> State -> States
statesThroughEpsilon r state = nub [ n | EpsilonRule s n <- r, s == state]


-- Create epsilon closure from list of states based on rules
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
                          new_unexplored = unexplored `union` statesThroughEpsilon rules s \\ new_expored


-- z ktorych stavov, akym znakom, pravidla -> vysledok
-- should have epsilon closure on input
reachableBy :: States -> Char -> Rules -> States
reachableBy states by rules =
    sort $ nub $ concat [ eClosure [next] rules | Rule curr c next <- rules, c == by, curr `elem` states]


-- type RuleTableRow = (States, [States])
type RuleTable a = [(a, [a])]
type RuleTableWithSets = RuleTable States
type RuleTableSimple = RuleTable State

makeRuleTableWithSets rules alphabet start = makeRuleTable' rules (sort alphabet) [eClosure [start] rules] []

-- alphabet should be sorted
makeRuleTable' :: Rules -> Alphabet -> [States] -> RuleTableWithSets -> RuleTableWithSets
makeRuleTable' _ _ [] table = table
makeRuleTable' rules alphabet [unexplored] table =
    let now_generated_states = map (\c -> reachableBy unexplored c rules) alphabet
        new_row = (unexplored, now_generated_states)
        new_table = table ++ [new_row]
        explored = map fst new_table
        new_unexplored = [x | x <- now_generated_states, x `notElem` explored && (x /= [])]
    
    -- all states are explored and no new unexplored states were generated
    in  if null new_unexplored
            then new_table
            else makeRuleTable' rules alphabet new_unexplored new_table
makeRuleTable' rules alphabet (u_fst:u_rest) table = 
    let t_after_fst = makeRuleTable' rules alphabet [sort u_fst] table
    in  nub $ makeRuleTable' rules alphabet u_rest t_after_fst  -- TODO find why nub needed

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


newFinalStates old new m =
    [Map.findWithDefault (- 1) n m | n <- new, not (null $ intersect old n)]
