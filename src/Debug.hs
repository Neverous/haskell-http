module Debug where

import Control.Monad.State
import Control.Monad.Trans
import Data.Time
import System.IO

type DebugIO = StateT Int IO

runDebugIO :: Int -> DebugIO a -> IO a
runDebugIO level debug = do
    (io, _) <- runStateT debug level
    return io

level_to_string :: Int -> String
level_to_string 1 = "DEBUG  "
level_to_string 2 = "NOTICE "
level_to_string 3 = "INFO   "
level_to_string 4 = "WARNING"
level_to_string 5 = "ERROR  "
level_to_string _ = "UNKNOWN"

string_to_level :: String -> Int
string_to_level "debug"   = 1
string_to_level "notice"  = 2
string_to_level "info"    = 3
string_to_level "warning" = 4
string_to_level "error"   = 5
string_to_level "silent"  = 6
string_to_level _         = 0

set_verbosity :: Int -> DebugIO ()
set_verbosity level = put level

get_verbosity :: DebugIO Int
get_verbosity = get

message :: Int -> String -> DebugIO ()
message level message = do
    current_level <- get
    if current_level > level then return ()
    else liftIO $ do
        time <- getZonedTime
        putStrLn $ ((show time) ++
                    " " ++
                    (level_to_string level) ++
                    " " ++
                    message)

debug :: String -> DebugIO ()
debug msg = message 1 msg

notice :: String -> DebugIO ()
notice msg = message 2 msg

info :: String -> DebugIO ()
info msg = message 3 msg

warning :: String -> DebugIO ()
warning msg = message 4 msg

error :: String -> DebugIO ()
error msg = message 5 msg
