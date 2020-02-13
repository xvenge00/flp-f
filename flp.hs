import System.IO     

splitBy delimiter = foldr f [[]] 
    where f c l@(x:xs) | c == delimiter = []:l
                       | otherwise = (c:x):xs

-- read first line
-- states <- split by commas

-- read second line
-- alphabet <- chars are already separated in char[]

-- read third line
-- starts_state <- whole line is state name
-- check that it is contained in list of states

-- read fourth line
-- final_states <- get list separated by commas
-- check that all are contained in 

-- recoursively read line until EOF
-- on each line call split by commas
-- check states in states
-- check char in alphabet
-- rules <- append every rule

-- TODO algoritmus

-- zapis

-- foo <- readFile "test.in"

foo :: String -> String
foo x = x

determinize :: FSA -> FSA
determinize dka = dka

data FSA = FSA {
    states::[String],
    alphabet::[Char],
    start_state::String,
    final_states::[String],
    rules::[(String, Char, String)]   -- TODO (String, Char, String)
}


main = do  
    contents <- readFile "test.in"
    let content = splitBy '\n' contents
    
    let rka = FSA{states = splitBy ',' (content !! 0), alphabet = content !! 1, start_state = content !! 2, final_states = splitBy ',' (content !! 3), rules = splitBy ',' (drop 4 content)}
    -- let rka = (splitBy ',' (content !! 0), content !! 1, content !! 2, splitBy ',' (content !! 3), drop 4 content)

    return rka
    -- putStrLn contents
    -- return contents
    -- putStr contents