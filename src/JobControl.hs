{-
 - Copyright (c) 2010, Terrence Cole.
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

