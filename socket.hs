import Network
import System.IO
import Text.Printf
import Data.List
import System.Exit

server = "chat.freenode.org"
port   = 6667
chan   = "#agni-test"
nick   = "agni"
pass = "fireburner"
desc   = "Fire Fox"

main = do
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    write h "PASS" pass
    write h "NICK" nick
    write h "USER" (nick++" agni.lin.anticlack.com * :"++desc)
    write h "JOIN" chan
    listen h
write :: Handle -> String -> String -> IO ()
write h s t = do
    hPrintf h "%s %s\r\n" s t
    printf    "> %s %s\n" s t

eval :: Handle -> String -> IO ()
eval h      "!lip"                   = write h "QUIT" ":Left in peace" >> exitWith ExitSuccess
-- eval h  x | "!mirror" `isPrefixOf` x = privmsg h (drop 4 x)
eval _    _                          = return ()

listen :: Handle -> IO ()
listen h = forever $ do
    t <- hGetLine h
    let s = init t
    putStrLn s
    if ping s then pong s else eval h (clean s)
  where
    forever a = do a; forever a;
    clean     = drop 1 . dropWhile (/= ':') . drop 1
    ping x    = "PING :" `isPrefixOf` x
    pong x    = write h "PONG" (':' : drop 6 x)
