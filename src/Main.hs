{-
 - Copyright (c) 2010, Terrence Cole.
 -}
module Main where
import Parser
import JobControl
--import Control.Monad
--import System.Environment


interactiveGetLine :: IO String
interactiveGetLine = getLine


main :: IO()
main = do
    line <- interactiveGetLine

    let cmd = parseCommandLine line
    case cmd of
       	Left err -> putStrLn (show err)
       	Right cmd -> dispatchJob cmd

    main

