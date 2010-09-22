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
module Parser where
import Text.ParserCombinators.Parsec hiding (spaces, token)

-- Values for WordDesc flags
data WordDescFlags = W_HasDollar
                   | W_Assignment
                   | W_Quoted
                   | W_GlobExp
                   deriving (Show)

-- One chunk of input text
data WordDesc = WordDesc { 
                wordDescFlags :: [WordDescFlags],
                wordDescWord  :: String
              } deriving (Show)

-- The target of redirection
data Redirectee = RedirecteeFD Int
                | RedirecteeName String
                deriving (Show)

-- possible types for redirection
data RedirectInstruction = RedirOutputDirection
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
                         deriving (Show)

-- describes a redirection
data Redirect = Redirect { 
                redirectSource :: Int, -- source for redirection
                redirectInstruction :: RedirectInstruction,
                redirectDestination :: Redirectee
              } deriving (Show)

-- Possible values for command->flags.
data CommandFlags = CmddWantSubshell    -- User wants a subshell: ( command )
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

--FIXME: support for Subshells
data Command = SimpleCommand {
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


spaces :: Parser ()
spaces = skipMany1 space

optspaces :: Parser ()
optspaces = skipMany space

parseQuoted :: Parser WordDesc
parseQuoted =  do 
        char '"'
        x <- many (noneOf "\"")
        char '"'
        return $ WordDesc [W_Quoted] x

parseToken :: Parser WordDesc
parseToken = do
        x <- many1 (noneOf " \t\n><&;|")
        -- if the token contains =, then mark as assignment
        case any (\c -> c == '=') x of
            True -> return $ WordDesc [W_Assignment] x
            False -> return $ WordDesc [] x

parseWord :: Parser WordDesc
parseWord = do
        x <- try parseQuoted <|> parseToken
        return $ x

parseWordList :: Parser [WordDesc]
parseWordList = do
        words <- sepEndBy parseWord spaces
        return words

parseRedirection :: Parser Redirect
parseRedirection = 
    do filename <- try (string "<<<" >> optspaces >> parseWord)
       return $ Redirect 0 RedirReadingString (RedirecteeName (wordDescWord filename))
    <|>
    do filename <- try (string ">>" >> optspaces >> parseWord)
       return $ Redirect 1 RedirAppendingTo (RedirecteeName (wordDescWord filename))
    <|>
    do filename <- try (string "<<-" >> optspaces >> parseWord)
       return $ Redirect 0 RedirDeblankReadingUntil (RedirecteeName (wordDescWord filename))
    <|>
    do filename <- try (string "<<" >> optspaces >> parseWord)
       return $ Redirect 0 RedirReadingUntil (RedirecteeName (wordDescWord filename))
    <|>
    do try (string "<&-")
       return $ Redirect 0 RedirCloseThis (RedirecteeFD 0)
    <|>
    do fd <- try (string "<&" >> optspaces >> many1 digit)
       return $ Redirect 0 RedirDuplicatingInput (RedirecteeFD (read fd))
    <|>
    do filename <- try (string "<&" >> optspaces >> parseWord)
       return $ Redirect 0 RedirDuplicatingInputWord (RedirecteeName (wordDescWord filename))
    <|>
    do try (string ">&-")
       return $ Redirect 0 RedirCloseThis (RedirecteeFD 0)
    <|>
    do fd <- try (string ">&" >> optspaces >> many1 digit)
       return $ Redirect 1 RedirDuplicatingOutput (RedirecteeFD (read fd))
    <|>
    do filename <- try (string ">&" >> optspaces >> parseWord)
       return $ Redirect 1 RedirDuplicatingOutputWord (RedirecteeName (wordDescWord filename))
    <|>
    do filename <- try (string ">|" >> optspaces >> parseWord)
       return $ Redirect 0 RedirOutputForce (RedirecteeName (wordDescWord filename))
    <|>
    do filename <- try (string "<>" >> optspaces >> parseWord)
       return $ Redirect 0 RedirInputOutput (RedirecteeName (wordDescWord filename))
    <|>
    do filename <- try (char '>' >> optspaces >> parseWord)
       return $ Redirect 1 RedirOutputDirection (RedirecteeName (wordDescWord filename))
    <|>
    do filename <- try (char '<' >> optspaces >> parseWord)
       return $ Redirect 0 RedirInputDirection (RedirecteeName (wordDescWord filename))
    <|>
    do filename <- try (string "&>>" >> optspaces >> parseWord)
       return $ Redirect 0 RedirAppendErrAndOut (RedirecteeName (wordDescWord filename))
    <|>
    do filename <- try (string "&>" >> optspaces >> parseWord)
       return $ Redirect 0 RedirErrAndOut (RedirecteeName (wordDescWord filename))
    <|>
    do fd <- many1 digit
       redir <- parseRedirection
       return $ Redirect (read fd) (redirectInstruction redir) (redirectDestination redir)

parseRedirectionList :: Parser [Redirect]
parseRedirectionList = do
        redirs <- sepEndBy parseRedirection optspaces
        return redirs

parseSimpleCommand :: Parser Command
parseSimpleCommand = do
        words <- parseWordList
        redirs <- optspaces >> parseRedirectionList
        return $ SimpleCommand [] words redirs

parseCommandSep :: Parser String
parseCommandSep = 
       try (string "&&") <|> 
       try (string "||") <|> 
       try (string "|") <|> 
       try (string ";") <|> 
       try (string "&")

parseConnectionCommand :: Parser Command
parseConnectionCommand = do
        a <- parseSimpleCommand
        conn <- (optspaces >> parseCommandSep)
        b <- (optspaces >> parseCommandList)
        return $ ConnectionCommand [] a b conn

parseCommandList :: Parser Command
parseCommandList = do
        cmd <- try parseConnectionCommand <|> parseSimpleCommand
        eof
        return cmd

parseCommandLine :: String -> Either ParseError Command
parseCommandLine input = parse parseCommandList "shell" input

