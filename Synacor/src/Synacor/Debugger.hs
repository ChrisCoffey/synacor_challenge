module Synacor.Debugger where

import Control.Monad
import Control.Monad.Fix                    (fix)
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import qualified Data.Map                   as M
import Data.Word
import Network.Socket

import System.Exit                          (die)
import System.IO

import Synacor.Machine
import Synacor.Interpreter                  (writeTo)

data Vm = Vm { semaphor :: MVar CurrentState, machine :: CurrentState }
type Msg = String
data Cmd = 
    Pause 
    | Go 
    | Quit
    | Break Word16
    | Step
    | DumpReg
    | MemDump String
    | LoadSave String
    | SetR  Word16 Word16
    deriving (Show, Eq)

parseCmd :: String -> Maybe Cmd
parseCmd "quit" = Just Quit
parseCmd "pause" = Just Pause
parseCmd "go" = Just Go
parseCmd "step" = Just Step
parseCmd "dumpreg" = Just DumpReg
parseCmd xs = let
    ws = words xs
    in f ws where
        f ["memdump", f] = Just $ MemDump f
        f ["load", f] = Just $ LoadSave f
        f ["break", n] = Just $ Break (read n ::Word16)
        f ["set", r, v] = Just $ SetR (read r :: Word16) (read v :: Word16)
        f _ = Nothing

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

handleDebug :: MVar Cmd -> (CurrentState, CurrentState) -> IO CurrentState
handleDebug mvar (before, after) = do
    c <- takeMVar mvar
    case c of   Go        -> putMVar mvar c >> return after
                Pause     -> do 
                            putMVar mvar c
                            print . map (\i-> (memory before) M.! i) $ registers
                            threadDelay (10^6 * 3)
                            return before
                Step       -> do
                            putMVar mvar Step
                            print "Inst"
                            print . inst $ after
                            print "Registers" 
                            print . map (\i-> (memory after) M.! i) $ registers
                            print "Stack" 
                            print . stack $ after
                            threadDelay (10^6 * 5)
                            return after
                (SetR r v) -> do
                            putMVar mvar Pause
                            let tweaked = CurrentState {
                                inst = inst before,
                                stack = stack before,
                                memory = writeTo r v (memory before)
                            }
                            return tweaked
                (Break i')  -> if i' == (inst before)
                                then putMVar mvar Pause >> return before
                                else putMVar mvar (Break i') >> return after
                (MemDump f) -> do
                             putMVar mvar Go
                             writeFile f (show after)
                             return after
                (LoadSave f) -> do
                             putMVar mvar Go
                             a <- readFile f
                             let s = read a :: CurrentState
                             return s
                Quit       -> do { die "Received quit command"}
    
