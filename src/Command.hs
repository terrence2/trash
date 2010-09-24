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
module Command where


-- Values for WordDesc flags
data WordDescFlags = 
          W_HasDollar
        | W_Assignment
        | W_Quoted
        | W_GlobExp
        deriving (Eq, Show)

-- One chunk of input text
data WordDesc = 
        WordDesc { 
          wordDescFlags :: [WordDescFlags]
        , wordDescWord  :: String
        } deriving (Eq, Show)

-- The target of redirection
data Redirectee = 
          RedirecteeFD Int
        | RedirecteeName String
        deriving (Eq, Show)

-- possible types for redirection
data RedirectInstruction = 
          RedirOutputDirection
        | RedirInputDirection
        | RedirInputADirection
        | RedirAppendingTo
        | RedirReadingUntil
        | RedirReadingString
        | RedirDuplicatingInput
        | RedirDuplicatingOutput
        | RedirDeblankReadingUntil
        | RedirCloseThis
        | RedirErrAndOut
        | RedirInputOutput
        | RedirOutputForce
        | RedirDuplicatingInputWord
        | RedirDuplicatingOutputWord
        | RedirMoveInput
        | RedirMoveOutput
        | RedirMoveInputWord
        | RedirMoveOutputWord
        | RedirAppendErrAndOut
        deriving (Eq, Show)

-- describes a redirection
data Redirect = 
        Redirect { 
          redirectSource :: Int -- source for redirection
        , redirectInstruction :: RedirectInstruction
        , redirectDestination :: Redirectee
        } deriving (Show)

-- Possible values for command->flags.
data CommandFlags = 
          CmddWantSubshell    -- User wants a subshell: ( command )
        | CmdForceSubshell    -- Shell needs to force a subshell.
        | CmdInvertReturn     -- Invert the exit value.
        | CmdIgnoreReturn     -- Ignore the exit value.  For set -e.
        | CmdInhibitExpansion -- Do not expand the command words.
        | CmdNoFork           -- Don't fork; just call execve
        | CmdTimePipeline     -- Time a pipeline
        | CmdTimePosix        -- time -p; use POSIX.2 time output spec.
        | CmdAmpersand        -- command &
        | CmdStdinRedir       -- async command needs implicit </dev/null
        | CmdCommandBuiltin   -- command executed by `command' builtin
        | CmdCoprocSubshell
        deriving (Show)

-- The top-level command structure
data Command = 
        SimpleCommand {
          commandFlags :: [CommandFlags]
        , commandWords :: [WordDesc]     -- env override, command, args, etc
        , commandRedirects :: [Redirect] -- redirects for the command
        }
        | ConnectionCommand {
          commandFlags :: [CommandFlags]
        , commandFirst :: Command
        , commandSecond :: Command
        , commandConnector :: String -- the separating string, |, ;, &&, ||, etc
        }
        | SubshellCommand {
          commandFlags :: [CommandFlags]
        , commandSub :: Command
        }
        deriving (Show)

