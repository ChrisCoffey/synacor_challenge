module Synacor.Debugger where

import Control.Concurrent.MVar

data Dbg = Dbg { semaphor :: MVar CurrentState, machine :: CurrentState }
data Cmd = Pause | Go

parseCmd :: String -> Maybe Cmd
parseCmd "pause" = Just Pause
parseCmd "go" = Just Go
parseCmd _ = Nothing

handleCmd :: Dbg -> Cmd -> Dbg
