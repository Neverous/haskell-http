module Server where

import Config (
    Config
    )

import Network.Socket (
    withSocketsDo
    )

import Options (
    Options
    )

data Server =
    Server {
        socket :: Int
    }
    deriving Show

new_server :: Options -> Config -> IO Server
new_server options config = return Server { socket = 7 }

run_server :: Server -> IO ()
run_server server = return ()
