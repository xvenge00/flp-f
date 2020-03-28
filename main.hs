module Main (main) where

import System.Environment (getArgs)
import System.Exit (die)

import FSATypes
import FSAParser
import FSADeterminize

main :: IO ()
main = do
    (action, input) <- procArgs =<< getArgs
    either die action (j2r $ parse2FSA input)

procArgs :: [String] -> IO (FSA -> IO (), String)
procArgs [x] = do
    input <- getContents
    case x of
     "-i" -> return (dumpFSA, input)
     "-t" -> return (determinizeFSA, input)
     _    -> die ("unknown option " ++ x)
procArgs [x,y] = do
    input <- readFile y
    case x of
     "-i" -> return (dumpFSA, input)
     "-t" -> return (determinizeFSA, input)
     _    -> die ("unknown option " ++ x)
procArgs _ = die "expecting two arguments: [-i|-t] [FILE]"

dumpFSA :: FSA -> IO ()
dumpFSA fsa = do
    putStrLn (showFSA fsa)

determinizeFSA :: FSA -> IO ()
determinizeFSA fsa = do
    putStrLn $ showFSA $ determinize $ fsa

j2r (Just x) = Right x
j2r _ = Left "ahoj"
