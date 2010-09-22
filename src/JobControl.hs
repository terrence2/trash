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
module JobControl where
import Parser
import System.Posix.Process


dispatchJob :: Command -> IO ()
dispatchJob (SimpleCommand flags words redirs) = do
        let wrapper = childMain words redirs
        child_id <- forkProcess wrapper
        child_status <- getProcessStatus True True child_id
        return ()
dispatchJob _ = return ()


childMain :: [WordDesc] -> [Redirect] -> IO ()
childMain words redirs = do
        let args = map wordDescWord words
        rv <- executeFile (args !! 0) True (tail args) Nothing
        return ()

