-- Toying around with Haskell
-- Based on https://wiki.haskell.org/Roll_your_own_IRC_bot
-- M. Meeuwisse

import Data.Char
import Data.List
import Data.List.Split
import Network
import System.IO
import Control.Monad.Reader
import Control.Concurrent
import Control.Exception
import Text.Printf
import System.Console.ANSI

server = "irc.freenode.org"
chan   = "#tutbot-testing"
nick   = "replace"

chanMatch = " PRIVMSG "
chanSpeak = (++) (chanMatch ++ chan ++ " :")

dropInt :: (Num a, Eq a) => a -> [b] -> [b]
dropInt _ [] = []
dropInt 0 b  = b
dropInt a (b:b') = dropInt (a - 1) b'

type Net = ReaderT Bot IO
data Bot = Bot { socket :: Handle }

main :: IO ()
main = bracket connect (hClose . socket) (runReaderT run)

connect :: IO Bot
connect = do
    h <- connectTo server (PortNumber 6667)
    forkIO . forever $ getLine >>= hPrintf h "%s\r\n" . chanSpeak
    hSetBuffering h NoBuffering
    return (Bot h)

run :: Net ()
run = do
    write ("NICK " ++ nick)
    write ("USER " ++ nick ++ " 0 * :haskell newb")
    write ("JOIN " ++ chan)
    asks socket >>= forever . (=<<) (parse . init) . liftIO . hGetLine

parse :: String -> Net ()
parse s
    | ping s = pong s
    | chanMatch `isInfixOf` s = tell True $ format s
    | " JOIN " `isInfixOf` s = tell False $ joined s 
    | " QUIT " `isInfixOf` s = tell False $ quit s 
    | otherwise = tell False s
    where
        ping = isPrefixOf "PING"
        pong = write . (++) "PO" . drop 2
        tell bright a = if bright then
            liftIO (setSGR [SetConsoleIntensity NormalIntensity] >> (putStrLn a)) else
            liftIO (setSGR [SetConsoleIntensity FaintIntensity ] >> (putStrLn a))

format :: String -> String
format s = (tail a) ++ " > " ++ (concat d) where
	(a: b) = splitOn "!" s
	(_: c) = splitOn chanMatch (head b)
	d = [dropInt (length chan + 2) $ head c] ++ (map ((++) chanMatch) $ tail c) -- undo remaining splits

joined :: String -> String
joined s = (tail a) ++ " joined (" ++ c ++ ")" where
    (a: b) = splitOn "!" s
    (c: _) = splitOn " JOIN " (head b)

quit :: String -> String
quit s = (tail a) ++ " quit (" ++ (head c) ++ ")" where
    (a: b) = splitOn "!" s
    (_: c) = splitOn " QUIT :" (head b)

write :: String -> Net ()
write s = do
    h <- asks socket
    liftIO $ hPrintf h "%s\r\n" s
