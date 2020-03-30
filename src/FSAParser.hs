{-# LANGUAGE RecordWildCards #-}

module FSAParser where

import Control.Applicative ((<|>))
import Control.Arrow (left)
import Control.Monad ((<=<))
import Text.Parsec (parse, newline, digit, char, satisfy, sepBy, endBy, many1, try, many)
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
statesP = sepBy stateP comma

stateP :: Parser State
stateP = read <$> many1 digit

alphabetP :: Parser Alphabet
alphabetP = many symbP

symbP :: Parser Symbol
-- TODO only alphabet
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
