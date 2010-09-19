{-
 - Copyright (c) 2010, Terrence Cole.
 -}
module Main where
import Interact
import Parser
import JobControl


mainloop :: IO()
mainloop = do
        line <- interactiveGetLine

        let cmd = parseCommandLine line
        case cmd of
           	Left err -> putStrLn (show err)
           	Right cmd -> dispatchJob cmd

        mainloop


main :: IO()
main = do
        interactiveSetup
        mainloop

