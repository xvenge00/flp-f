module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))

import FSATypes
import FSAParser
import FSADeterminize

die msg = putStrLn msg >> exitWith (ExitFailure 1)

main :: IO ()
main = do
    (action, input) <- procArgs =<< getArgs
    either die action (parseFSA input)

procArgs :: [String] -> IO (FSA -> IO (), String)
procArgs [x] = do
    input <- getContents
    getAction x input
procArgs [x,y] = do
    input <- readFile y
    getAction x input
procArgs _ = die "expecting two arguments: [-i|-t] [FILE]"

getAction a input =
    either die (\action -> return (action, input)) (getAction' a)
        where getAction' x = 
                case x of
                    "-i" -> Right dumpFSA
                    "-t" -> Right determinizeFSA
                    _    -> Left ("unknown option " ++ x)

dumpFSA :: FSA -> IO ()
dumpFSA fsa = putStr (showFSA fsa)

determinizeFSA :: FSA -> IO ()
determinizeFSA fsa = putStr $ showFSA $ determinize fsa
