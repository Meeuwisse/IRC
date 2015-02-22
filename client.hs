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
    --hSetEncoding h utf8
    return (Bot h)

run :: Net ()
run = do
    write ("NICK " ++ nick)
    write ("USER " ++ nick ++ " 0 * :haskell newb")
    write ("JOIN " ++ chan)
    asks socket >>= forever . (=<<) (parse . init) . liftIO . hGetLine

parse :: String -> Net ()
parse s
    | chanMatch `isInfixOf` s = tell True $ format s
    | " JOIN " `isInfixOf` s = tell False $ joined s
    | " PART " `isInfixOf` s = tell False $ parted s
    | " QUIT " `isInfixOf` s = tell False $ quit s
    | "PING " `isPrefixOf` s = (write . (++) "PO" . drop 2) s
    | otherwise = tell False s
    where -- Print colored line, then force terminal back to default
        tell as str = liftIO $ setBright as >> putStr str >> setBright True >> putStrLn ""

setBright :: Bool -> IO ()
setBright True = setSGR [SetConsoleIntensity NormalIntensity]
setBright False = setSGR [SetConsoleIntensity FaintIntensity]

format :: String -> String
format s
    | "ACTION " `isPrefixOf` d = "> " ++ (tail a) ++ (drop 6 d)
    | otherwise = (tail a) ++ " > " ++ d
    where
	   (a: b) = splitOn "!" s
	   (_: c) = splitOn chanMatch (head b)
       -- Trim channel id & undo remaining splits
	   d = concat $ [dropInt (length chan + 2) $ head c] ++ (map ((++) chanMatch) $ tail c)

reason:: [String] -> String
reason optional | (x:_) <- optional = " (" ++ x ++ ")" | otherwise = []

joined :: String -> String
joined s = (tail a) ++ " joined" ++ (reason c) where
    (a: b) = splitOn "!" s
    c = splitOn " JOIN " (head b)

parted :: String -> String
parted s = (tail a) ++ " left" ++ (reason c) where
    (a: b) = splitOn "!" s
    (_: c) = splitOn " PART :" (head b)

quit :: String -> String
quit s = (tail a) ++ " quit" ++ (reason c) where
    (a: b) = splitOn "!" s
    (_: c) = splitOn " QUIT :" (head b)

write :: String -> Net ()
write s = do
    h <- asks socket
    liftIO $ hPrintf h "%s\r\n" s
