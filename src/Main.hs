{-
 - Copyright (c) 2010, Terrence Cole.
 -
 - This file is part of Trash, Terrence's Re-Bourne Again SHell.
 -
 - Trash is free software: you can redistribute it and/or modify
 - it under the terms of the GNU General Public License as published by
 - the Free Software Foundation, either version 3 of the License, or
 - (at your option) any later version.
 -
 - Trash is distributed in the hope that it will be useful,
 - but WITHOUT ANY WARRANTY; without even the implied warranty of
 - MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 - GNU General Public License for more details.
 -
 - You should have received a copy of the GNU General Public License
 - along with Trash.  If not, see <http://www.gnu.org/licenses/>.
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
           	Right cmd -> dispatchCommand cmd

        mainloop


main :: IO()
main = do
        interactiveSetup
        mainloop

