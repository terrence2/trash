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
import Command
import Parser
import System.Exit
import System.Posix.IO
import System.Posix.Process
import System.Posix.Types


builtins = [
        -- directory commands
        "cd", "pushd", "popd", "back", "forward", "up", "down", "next", "previous",
        -- env command
        "export", "unset"
        ]

data Direction = GoLeft | GoRight


dispatchCommand :: Command -> IO (Maybe ProcessStatus)
dispatchCommand (SimpleCommand flags words redirs) = do
        let (wEnv, wTgt, wArgs) = splitCommand words
        if elem (wordDescWord wTgt) builtins
            then execBuiltinCommand (SimpleCommand flags words redirs)
            else execSimpleCommand (SimpleCommand flags words redirs)
-- run two commands back-to-back
dispatchCommand (ConnectionCommand flags first second ";") = do
        a <- dispatchCommand first
        b <- dispatchCommand second
        return b
-- only run second if first succeeds
dispatchCommand (ConnectionCommand flags first second "&&") = do
	a <- dispatchCommand first
        case a of
            Just rv -> case rv of
                Exited ExitSuccess -> dispatchCommand second
                _ -> return a
            Nothing -> return Nothing
-- only run second if first fails
dispatchCommand (ConnectionCommand flags first second "||") = do
        a <- dispatchCommand first
        case a of
            Just rv -> case rv of
                Exited ExitSuccess -> return a
                _ -> dispatchCommand second
            Nothing -> return Nothing
{-
dispatchCommand (ConnectionCommand flags first second "|") = do
        (readFd, writeFd) <- createPipe
        let writer = Redirect 1 RedirOutputDirection (RedirecteeFD (fromIntegral writeFd))
        let reader = Redirect 0 RedirInputDirection (RedirecteeFD (fromIntegral readFd))
        putStrLn "1"
        dispatchCommand (addRedirect GoRight writer first)
        putStrLn "2"
        dispatchCommand (addRedirect GoLeft reader second)
        putStrLn "3"
        return ()
-}
dispatchCommand _ = return Nothing


execSimpleCommand :: Command -> IO (Maybe ProcessStatus)
execSimpleCommand (SimpleCommand flags words redirs) = do
        let wrapper = childMain words redirs
        child_id <- forkProcess wrapper
        child_status <- getProcessStatus True True child_id
        return child_status



builtinUnrecognized = do
        putStrLn "Unrecognized command"
        return $ Just $ Exited ExitSuccess

execBuiltinCommand :: Command -> IO (Maybe ProcessStatus)
execBuiltinCommand (SimpleCommand flags words redirs) = do
        let (wEnv, wTgt, wArgs) = splitCommand words
        case (wordDescWord wTgt) of
                "cd" -> return $ Just $ Exited ExitSuccess
                _ -> builtinUnrecognized


addRedirect ::  Direction -> Redirect -> Command -> Command
addRedirect _ redir (SimpleCommand flags words redirs) = SimpleCommand flags words (redirs ++ [redir])
addRedirect GoLeft redir (ConnectionCommand flags left _ _) = addRedirect GoLeft redir left
addRedirect GoRight redir (ConnectionCommand flags _ right _) = addRedirect GoRight redir right


-- Splits out he env overrides, the command target and command args
-- FIXME: right now this just returns the first word, this need to skip env overrides
splitCommand :: [WordDesc] -> ([WordDesc], WordDesc, [WordDesc])
splitCommand (w:ws) = ([], w, ws)

childMain :: [WordDesc] -> [Redirect] -> IO ()
childMain words redirs = do
        let (wEnv, wTgt, wArgs) = splitCommand words
        let args = map wordDescWord wArgs
        childSetupRedirects redirs
        rv <- executeFile (wordDescWord wTgt) True args Nothing
        return ()

childSetupRedirects :: [Redirect] -> IO ()
childSetupRedirects [] = return ()
childSetupRedirects (r:rs) = do
        childSetupRedirect r
        childSetupRedirects rs

childSetupRedirect :: Redirect -> IO ()
childSetupRedirect (Redirect src inst (RedirecteeFD dst))  = do
        let srcFd = Fd (fromIntegral src)
        let dstFd = Fd (fromIntegral dst)
        childSetupRedirectFd dstFd srcFd
childSetupRedirect (Redirect src inst (RedirecteeName name))  = do
        --openFd :: FilePath -> OpenMode -> Maybe FileMode -> OpenFileFlags -> IO Fd
        let srcFd = Fd (fromIntegral src)
        let (openMode, openFlags) = argsForInst inst
        dstFd <- openFd name openMode (Just 0o777) openFlags
        childSetupRedirectFd dstFd srcFd

argsForInst :: RedirectInstruction -> (OpenMode, OpenFileFlags)
--                                             (OpenFileFlags append excl noctty nonblock trunc)
argsForInst RedirOutputDirection = (WriteOnly, (OpenFileFlags False False True False True))
argsForInst RedirInputDirection  = (ReadOnly,  (OpenFileFlags False False True False True))

-- open new fd on top of old fd and close the old copy
childSetupRedirectFd :: Fd -> Fd -> IO ()
childSetupRedirectFd oldFd newFd = do
       	newFd <- dupTo oldFd newFd
        --closeFd oldFd
        return ()


