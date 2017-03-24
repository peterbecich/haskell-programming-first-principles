module Main where

import Control.Monad (forever)
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import Data.ByteString.Char8 (pack)

logAndEcho :: Socket -> IO ()
logAndEcho sock = forever $ do
  _ <- putStrLn "open socket"
  (soc, _) <- accept sock
  -- _ <- printAndKickback sock
  _ <- do
    _ <- putStrLn "recv"
    msg <- recv soc 1024
    _ <- print msg
    _ <- putStrLn "sendAll"
    sendAll soc msg
  _ <- putStrLn "close socket"
  close soc
--   where printAndKickback connection = do
--           _ <- putStrLn "recv"
--           msg <- recv connection 1024
--           _ <- print msg
--           _ <- putStrLn "sendAll"
--           -- sendAll connection (pack "foo")
--           sendAll connection msg

main :: IO ()
main = withSocketsDo $ do
  _ <- putStrLn "starting debugger"
  addrinfos <- getAddrInfo
               (Just (defaultHints
                     { addrFlags = [AI_PASSIVE]})
               )
               Nothing (Just "79")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  _ <- bind sock (addrAddress serveraddr)
  _ <- listen sock 1
  _ <- forever $ logAndEcho sock
  _ <- putStrLn "shutting down"
  close sock
