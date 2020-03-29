module FSADeterminize (determinize) where

import Data.List (union, delete, (\\), sort, nub, intersect, intercalate, mapAccumL)
import qualified Data.Map.Strict as Map (fromList, findWithDefault)

import FSATypes

determinize :: FSA -> FSA
determinize old =
    let rule_table_sets = makeRuleTableWithSets (rules old) (alphabet old) (start_state old)
        rename_map = makeRenameMap rule_table_sets
        rule_table_renamed = renameTable rename_map rule_table_sets
        new_rules = makeRules rule_table_renamed (alphabet old)
        new_states = map fst rule_table_renamed
        new_alphabet = foldl (\acc r -> acc `union` [c r]) "" new_rules
        new_final = newFinalStates (final_states old) (map fst rule_table_sets) rename_map
        new_start = Map.findWithDefault 0 (eClosure [start_state old] (rules old)) rename_map
    in  FSA new_states new_alphabet new_start new_final new_rules

statesThroughEpsilon :: Rules -> State -> States
statesThroughEpsilon [] _ = []
statesThroughEpsilon [Rule {}] _ = []
statesThroughEpsilon [EpsilonRule s n] state = [n | s == state]
statesThroughEpsilon (r:rs) state = statesThroughEpsilon [r] state `union` statesThroughEpsilon rs state

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
                          new_unexplored = unexplored `union` statesThroughEpsilon rules s \\ new_expored


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
              reachableBy c (r:rs) = reachableBy c [r] `union` reachableBy c rs


-- type RuleTableRow = (States, [States])
type RuleTable a = [(a, [a])]
type RuleTableWithSets = RuleTable States
type RuleTableSimple = RuleTable State

makeRuleTableWithSets rules alphabet start = makeRuleTable' rules alphabet [eClosure [start] rules] []

-- alphabet should be sorted
makeRuleTable' :: Rules -> Alphabet -> [States] -> RuleTableWithSets -> RuleTableWithSets
makeRuleTable' _ _ [] table = table
makeRuleTable' rules alphabet [unexplored] table =
    let now_generated_states = map (\c -> reachableIn1 unexplored c rules) alphabet
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
makeRules table alphabet = foldl (\acc row -> acc ++ makeRuleFromRow row) [] table
    where   makeRuleFromRow :: (State, States) -> Rules
            makeRuleFromRow row = foldl (\acc s -> acc ++ uncurry (rule (fst row)) s) [] (zip alphabet (snd row))
            rule :: State -> Char -> State -> Rules
            rule from c to = [Rule from c to | to >= 0]

newFinalStates old new m =
    foldl (\acc n -> acc ++ ([Map.findWithDefault (- 1) n m | not (null $ intersect old n)])) [] new
