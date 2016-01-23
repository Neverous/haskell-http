module Server where

import Config (
    Config
    )

import Control.Monad (
    forever,
    )

import Data.Map (
    (!),
    )

import Debug (
    debug,
    notice,
    )

import Network (
    PortID,
    )

import Network.Socket (
    accept,
    bind,
    close,
    defaultProtocol,
    Family (
        AF_INET
        ),
    inet_addr,
    listen,
    socket,
    SockAddr (
        SockAddrInet
        ),
    Socket,
    SocketType (
        Stream
        ),
    withSocketsDo,
    )

data Server =
    Server {
        listen_socket :: Socket,
        verbosity :: Int,
        config :: Config
    }
    deriving Show

new_server :: Int -> Config -> IO Server
new_server verbosity config = do

    global <- return $ config ! "global"
    port <- return (read (global ! "port") :: Integer)
    host <- return $ global ! "host"
    addr <- inet_addr host

    debug verbosity "Creating socket"
    listen_socket <- socket AF_INET Stream defaultProtocol

    debug verbosity "Binding socket"
    bind listen_socket (SockAddrInet (fromInteger port) addr)

    debug verbosity "Listening on socket"
    listen listen_socket 8

    notice verbosity $ "Server setup on " ++ host ++ ":" ++ (show port)

    return Server {
        listen_socket   = listen_socket,
        verbosity       = verbosity,
        config          = config
        }

run_server :: Server -> IO ()
run_server server = forever $ do
    debug (verbosity server) "Listening for new connections"
    (socket, addr) <- accept (listen_socket server)
    notice (verbosity server) "Got new connection"
    handle_connection socket addr
    notice (verbosity server) "Processed new connection"
    close socket

handle_connection :: Socket -> SockAddr -> IO ()
handle_connection socket addr = return ()
