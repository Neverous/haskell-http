module Debug where

import Data.Time (
    getZonedTime
    )

import System.IO (
    putStr,
    putStrLn
    )

level_to_string :: Int -> String
level_to_string 1 = "DEBUG  "
level_to_string 2 = "NOTICE "
level_to_string 3 = "INFO   "
level_to_string 4 = "WARNING"
level_to_string 5 = "ERROR  "
level_to_string _ = "INFO   "

string_to_level :: String -> Int
string_to_level "debug"   = 1
string_to_level "notice"  = 2
string_to_level "info"    = 3
string_to_level "warning" = 4
string_to_level "error"   = 5
string_to_level "silent"  = 6
string_to_level _         = 3

choose_verbosity_level :: Int -> String -> Int
choose_verbosity_level 0 x = read x :: Int
choose_verbosity_level x _ = x

message :: Int -> Int -> String -> IO ()
message current_level level message =
    if current_level > level then return ()
    else do
        time <- getZonedTime
        putStr (show time)
        putStr " "
        putStr (level_to_string level)
        putStr " "
        putStrLn message

debug :: Int -> String -> IO ()
debug current_level msg = message current_level 1 msg

notice :: Int -> String -> IO ()
notice current_level msg = message current_level 2 msg

info :: Int -> String -> IO ()
info current_level msg = message current_level 3 msg

warning :: Int -> String -> IO ()
warning current_level msg = message current_level 4 msg

error :: Int -> String -> IO ()
error current_level msg = message current_level 5 msg
