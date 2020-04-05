{-|
  rka-2-dka
  author: Adam Venger (xvenge00)
  year: 2020
  
  Functional project FLP
-}

module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))

import FSATypes
import FSAParser
import FSADeterminize

-- entrypoint
main :: IO ()
main = do
    (action, input) <- procArgs =<< getArgs
    either die action (parseFSA input)

-- fail with message
die :: String -> IO a
die msg = putStrLn msg >> exitWith (ExitFailure 1)

-- process command line arguments
procArgs :: [String] -> IO (FSA -> IO (), String)
procArgs [x] = do
    input <- getContents
    getAction x input
procArgs [x,y] = do
    input <- readFile y
    getAction x input
procArgs _ = die "expecting two arguments: [-i|-t] [FILE]"

-- pick action based on input parameter
getAction :: String -> String -> IO (FSA -> IO (), String)
getAction a input =
    either die (\action -> return (action, input)) (getAction' a)
        where getAction' x = 
                case x of
                    "-i" -> Right dumpFSA
                    "-t" -> Right determinizeFSA
                    _    -> Left ("unknown option " ++ x)

-- only print representation of fsa from input
dumpFSA :: FSA -> IO ()
dumpFSA fsa = putStr (showFSA fsa)

-- determinize fsa from input
determinizeFSA :: FSA -> IO ()
determinizeFSA fsa = putStr $ showFSA $ determinize fsa
