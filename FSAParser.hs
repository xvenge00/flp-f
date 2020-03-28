module FSAParser (parse2FSA) where

import Data.List (union, delete, (\\), sort, nub, intersect, mapAccumL)
import qualified Data.Map.Strict as Map (fromList, findWithDefault)

import FSATypes

splitBy delimiter = foldr f [[]] 
    where f c l@(x:xs) | c == delimiter = []:l
                       | otherwise = (c:x):xs

parseState :: String -> State
parseState state = read state :: Integer


parseStates :: String -> States
parseStates states = map parseState (splitBy ',' states)

parseAlphabet :: String -> Alphabet
parseAlphabet alphabet = alphabet

parseRule :: [String] -> Maybe Rule
parseRule [state, [char], stateNext] = Just $ Rule (parseState state) char (parseState stateNext)
parseRule [state, [], stateNext] = Just $ EpsilonRule (parseState state) (parseState stateNext)
parseRule _ = Nothing

-- TODO move implementation into parseRules
parseRulesImpl :: [String] -> [Maybe Rule]
parseRulesImpl lines
    |lines == [] = []
    |otherwise = parseRule ( splitBy ',' $ head lines ) : (parseRulesImpl $ tail lines)

parseRules :: [String] -> Maybe Rules
parseRules lines = case sequence $ parseRulesImpl lines of
                    Just x -> Just $ x
                    Nothing -> Nothing
    
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

