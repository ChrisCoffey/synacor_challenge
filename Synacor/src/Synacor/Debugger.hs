module Synacor.Debugger where

import Control.Concurrent.MVar
import Network.Socket

import Synacor.Machine

data Vm = Vm { semaphor :: MVar CurrentState, machine :: CurrentState }
data Cmd = Pause | Go

parseCmd :: String -> Maybe Cmd
parseCmd "pause" = Just Pause
parseCmd "go" = Just Go
parseCmd _ = Nothing

    -- parse the input and update the mvar if necessary
    -- mvar is used to make the main thread do debugger work
    -- the main thread places the machine state into the MVar each pass
    -- the debugger grabs the mvar on pause & doesn't release untilgo is received
    --      memory can be rewritten during this time 

runDebugger :: IO ()
runDebugger = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock REuseAddr 1
    bindSocket sock (SockAddrInet 8888 iNADDR_ANY)
    listen sock 1
   
dbgLoop :: (Socket, SockAddr) -> IO ()
dbgLoop (sock, _) = do
    h <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    
