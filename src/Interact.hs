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

