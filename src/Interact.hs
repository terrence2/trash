{-
 - Copyright (c) 2010, Terrence Cole.
 -}
module Interact where
import System.IO


interactiveSetup :: IO ()
interactiveSetup = do
        hSetBuffering stdin NoBuffering
        hSetBuffering stdout NoBuffering

interactiveGetLine :: IO String
interactiveGetLine = do
        hPutStr stdout "hello> "
        ln <- getLine
        return ln

