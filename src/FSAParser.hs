{-|
  rka-2-dka
  author: Adam Venger (xvenge00)
  year: 2020
  
  Functional project FLP
-}

{-# LANGUAGE RecordWildCards #-}

module FSAParser (parseFSA) where

import Control.Applicative ((<|>), (<*>), (<$>), (<$), (<*))
import Control.Arrow (left)
import Control.Monad ((<=<))
import Data.List (sort)
import Text.Parsec (parse, newline, digit, char, satisfy, sepBy, endBy, many1, try, many)
import Text.Parsec.String (Parser)

import FSATypes

-- parse fsa from string to internal representation and validate it
parseFSA :: String -> Either String FSA
parseFSA = validate <=< left show . parse fsaParser ""

-- FSA parsec parser
fsaParser :: Parser FSA
fsaParser =
  FSA <$>
    statesP <* newline <*> 
    alphabetP <* newline <*>
    stateP <* newline <*>
    statesP <* newline <*>
    rulesP

-- states parsec parser
stateP :: Parser State
stateP = read <$> many1 digit
statesP :: Parser States
statesP = sort <$> sepBy stateP comma

-- alphabet parsec parser
alphabetP :: Parser Alphabet
alphabetP = sort <$> many symbP
symbP :: Parser Symbol
symbP = satisfy (`elem` ['a'..'z'])

-- rules parsec parser
rulesP :: Parser Rules
rulesP = sort <$> endBy ruleP newline
ruleP :: Parser Rule
ruleP = try ruleEpsilonP <|> ruleSymbolP 
ruleEpsilonP :: Parser Rule
ruleEpsilonP = EpsilonRule <$> stateP <* comma <* comma <*> stateP
ruleSymbolP :: Parser Rule
ruleSymbolP = Rule <$> stateP <* comma <*> symbP <* comma <*> stateP

-- comma parsec parser
comma :: Parser Char
comma = char ','

-- fsa validator
validate :: FSA -> Either String FSA
validate fsa@FSA{..} = if allOK then Right fsa else Left "invalid FSA"
  where
    allOK =
      start_state `elem` states &&
      all (`elem` states) final_states &&
      all ((`elem` states) . current) rules &&
      all ((`elem` states) . next) rules &&
      all (`elem` alphabet) [ c x | x@Rule {} <- rules] &&
      isUnique states &&
      isUnique final_states &&
      isUnique alphabet &&
      isUnique rules
    isUnique l = not $ someSame $ sort l
      where someSame (x:xs:rest) = x==xs || someSame (xs:rest)
            someSame _ = False
