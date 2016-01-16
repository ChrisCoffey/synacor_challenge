module Synacor.Debugger where

import Control.Monad
import Control.Monad.Fix                    (fix)
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Data.Word
import Network.Socket
import System.IO

import Synacor.Machine

data Vm = Vm { semaphor :: MVar CurrentState, machine :: CurrentState }
type Msg = String
data Cmd = 
    Pause 
    | Go 
    | Quit
    | Break Word16
    | Step
    | DumpReg 
    | SetR  Word16 Word16
    deriving (Show)

parseCmd :: String -> Maybe Cmd
parseCmd "quit" = Just Quit
parseCmd "pause" = Just Pause
parseCmd "go" = Just Go
parseCmd "step" = Just Step
parseCmd "dumpreg" = Just DumpReg
parseCmd xs = let
    ws = words xs
    in f ws where
        f ["break", n] = Just $ Break (read n ::Word16)
        f ["set", r, v] = Just $ SetR (read r :: Word16) (read v :: Word16)
        f _ = Nothing

    -- parse the input and update the mvar if necessary
    -- mvar is used to make the main thread do debugger work
    -- the main thread places the machine state into the MVar each pass
    -- the debugger grabs the mvar on pause & doesn't release untilgo is received
    --      memory can be rewritten during this time 

startDebugger :: MVar Cmd -> IO ()
startDebugger mvr = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bindSocket sock (SockAddrInet 8888 iNADDR_ANY)
    listen sock 1
    dbgLoop sock mvr
   
dbgLoop :: Socket -> MVar Cmd -> IO ()
dbgLoop sock mvr = do
    conn <- accept sock
    forkIO (runDebugger conn mvr)
    return ()
    
runDebugger :: (Socket, SockAddr) -> MVar Cmd -> IO ()
runDebugger (sock, _) mvr = do
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    hPutStrLn hdl "Synacor Debugger Online"
    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        line <- liftM init (hGetLine hdl)
        case parseCmd line of
            Nothing -> do
                hPutStrLn hdl "Invalid Command!"
                loop
            Just Quit -> do
                _ <- takeMVar mvr
                putMVar mvr Quit 
                hPutStrLn hdl "Received Quit Command"
            Just c -> do
                _ <- takeMVar mvr
                putMVar mvr c
                loop
