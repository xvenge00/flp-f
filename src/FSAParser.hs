{-# LANGUAGE RecordWildCards #-}

module FSAParser where

import Data.List (union, delete, (\\), sort, nub, intersect, mapAccumL)
import qualified Data.Map.Strict as Map (fromList, findWithDefault)

import Control.Applicative ((<$>), (<*>), (<$), (<*), (<|>))
import Control.Arrow (left)
import Control.Monad ((<=<))
import Text.Parsec (Parsec, parse,
        newline, digit, string, char, satisfy, sepBy1, endBy, many1, choice, optional, alphaNum, try)
import Text.Parsec.String (Parser)

import FSATypes

parseFSA :: String -> Either String FSA
parseFSA = validate <=< left show . parse fsaParser ""

fsaParser :: Parser FSA
fsaParser =
  FSA <$>
    statesP <* newline <*> 
    alphabetP <* newline <*>
    stateP <* newline <*>
    statesP <* newline <*>
    rulesP

statesP :: Parser States
statesP = sepBy1 stateP comma

stateP :: Parser State
stateP = read <$> many1 digit

alphabetP :: Parser Alphabet
alphabetP = many1 symbP

symbP :: Parser Symbol
symbP = satisfy (`notElem` " ,<>\n\t")

rulesP :: Parser Rules
rulesP = endBy ruleP newline

ruleP :: Parser Rule
ruleP = try ruleEpsilonP <|> ruleSymbolP 
ruleEpsilonP = EpsilonRule <$> stateP <* comma <* comma <*> stateP
ruleSymbolP = Rule <$> stateP <* comma <*> symbP <* comma <*> stateP

comma :: Parser Char
comma = char ','

validate :: FSA -> Either String FSA
validate fsa@FSA{..} = if allOK then Right fsa else Left "invalid FSA"
  where
    allOK =
      start_state `elem` states &&
      all (`elem` states) final_states &&
      all ((`elem` states) . current) rules &&
      all ((`elem` states) . next) rules &&
      all ((`elem` alphabet) . c) [ x | x@Rule {} <- rules]
