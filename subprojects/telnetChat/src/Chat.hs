module Chat where

-- https://wiki.haskell.org/Implement_a_chat_server

-- https://hackage.haskell.org/package/network-2.6.3.1/docs/Network-Socket.html

import Network.Socket
import System.IO
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad (liftM)
import Control.Monad.Fix (fix)

-- forkIO :: IO () -> IO ThreadId
mainLoop :: Socket -> Chan String -> IO ()
mainLoop sock chan = do
  conn <- accept sock
  _ <- forkIO (runConn conn chan)
  mainLoop sock chan

-- runConn :: (Socket, SockAddr) -> IO ()
-- runConn (sock, _) = do
--   _ <- send sock "Hello! \n"
--   close sock

runConn :: (Socket, SockAddr) -> Chan String -> IO ()
runConn (sock, _) chan = do
  let broadcast msg = writeChan chan msg
  hdl <- socketToHandle sock ReadWriteMode
  _ <- hSetBuffering hdl NoBuffering
  commLine <- dupChan chan
  _ <- forkIO $ fix $ \loop -> do
    line <- readChan commLine
    _ <- hPutStrLn hdl line
    loop
  fix $ \loop -> do
    line <- liftM init (hGetLine hdl)
    _ <- broadcast line
    loop

main :: IO ()
main = do
  sock <- socket AF_INET Stream 0
  _ <- setSocketOption sock ReuseAddr 1
  _ <- bind sock (SockAddrInet 4242 iNADDR_ANY)
  _ <- listen sock 2
  chan <- newChan
  mainLoop sock chan
