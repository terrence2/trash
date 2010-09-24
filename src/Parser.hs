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
module Parser (parseCommandLine) where
import Command
import Text.ParserCombinators.Parsec hiding (spaces, token)


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
        tok <- many1 (noneOf " \t\n><&;|")
        -- if the token contains =, then mark as assignment
        let assign_flag = if elem '=' tok then [W_Assignment] else []
        return $ WordDesc assign_flag tok

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

