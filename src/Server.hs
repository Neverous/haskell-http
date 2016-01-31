module Server where

import Config
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.State
import Data.ByteString.Char8
import Data.List
import Data.Map
import Debug
import HTTP.Parser
import HTTP.Request
import HTTP.Response
import Mime
import Network
import Network.Socket
import System.FilePath
import System.IO

type Server = StateT (Socket, Config) DebugIO

runServer :: Config -> DebugIO ()
runServer config = do
    listen_socket <- create_listen_socket config
    (debugio, _) <- runStateT acceptor (listen_socket, config)
    return debugio

forkServer :: Server () -> Server ThreadId
forkServer server = do
    -- copy state
    level           <- lift $ get_verbosity
    listen_socket   <- get_listen_socket
    config          <- get_config

    -- fork IO
    liftIO . forkIO $ runDebugIO level $ do
        -- put state back
        (debugio, _) <- runStateT server (listen_socket, config)
        return debugio

create_listen_socket :: Config -> DebugIO Socket
create_listen_socket config = do
    global  <- return $ config ! "global"
    port    <- return (read (global ! "port") :: Integer)
    host    <- return $ global ! "host"
    addr    <- liftIO $ inet_addr host

    debug "Creating socket"
    listen_socket <- liftIO $ socket AF_INET Stream defaultProtocol
    liftIO $ setSocketOption listen_socket ReuseAddr 1

    debug $ "Binding socket to " ++ host ++ ":" ++ (show port)
    liftIO $ bind listen_socket (SockAddrInet (fromInteger port) addr)

    debug "Starting listening"
    liftIO $ listen listen_socket 8

    notice $ "Server started on " ++ host ++ ":" ++ (show port)
    return listen_socket

get_listen_socket :: Server Socket
get_listen_socket = do
    (listen_socket, _) <- get
    return listen_socket

get_config :: Server Config
get_config = do
    (_, config) <- get
    return config

acceptor :: Server ()
acceptor = do
    lift $ debug "Waiting for new connections"
    listen_socket <- get_listen_socket
    forever $ do
        (socket, addr) <- liftIO $ Network.Socket.accept listen_socket
        lift $ notice $ "Got new connection from " ++ (show addr)
        forkServer $ handle_connection socket (show addr)
    return ()

handle_connection :: Socket -> String -> Server ()
handle_connection socket addr = do
    lift $ debug $ "Processing connection from " ++ addr
    handle <- liftIO $ socketToHandle socket ReadWriteMode
    liftIO $ hSetBuffering handle NoBuffering
    request_data <- liftIO $ System.IO.hGetContents handle
    lift $ debug $ "Read request from " ++ addr

    (code, headers, body) <- case parse_request request_data of
                -- successfully parsed request
                    Right request -> process_request addr request
                -- errors
                    Left err -> error_response 400 err

    headers <- extend_headers code headers body
    response <- return $ Response {
        body = body,
        code = code,
        HTTP.Response.headers = headers,
        HTTP.Response.protocol = "HTTP/1.1"
    }

    lift $ debug $ "RESPONSE: " ++ (show response)
    lift $ debug $ "Sending response to " ++ addr
    liftIO $ System.IO.hPutStr handle (show response) `catch` ignoreException
    liftIO (Data.ByteString.Char8.hPutStr handle (HTTP.Response.body response)
            `catch` ignoreException)
    liftIO $ hClose handle `catch` ignoreException
    lift $ notice $ "Processed connection from " ++ addr
    return ()

process_request :: String -> Request -> Server (Int, HTTP.Response.Headers, ByteString)
process_request addr request = do
    case (HTTP.Request.protocol request) of
        "HTTP/1.1"  -> process_http addr request
        "HTTP/1.0"  -> process_http addr request
        _           -> error_response 400 ("Unsupported protocol: " ++
                                           (HTTP.Request.protocol request))

process_http :: String -> Request -> Server (Int, HTTP.Response.Headers, ByteString)
process_http addr request = do
    lift $ info $ (method request) ++ " " ++ (uri request)
    case (method request) of
        "GET"   -> process_get addr request
        _       -> error_response 405 ""

error_response :: Int -> String -> Server (Int, HTTP.Response.Headers, ByteString)
error_response code body = return (
    code,
    fromList [("Content-Type", "text/html")],
    pack ("<html><head><title>" ++
          (code_to_string code) ++
          "</title></head><body>" ++
          body ++
          "</body></html>"))

process_get :: String -> Request -> Server (Int, HTTP.Response.Headers, ByteString)
process_get addr request = do
    config <- get_config
    hostname <- return $ (Data.Map.lookup "Host" (HTTP.Request.headers request))
    case hostname of
        Nothing -> error_response 400 "Missing host header"
        Just hostname -> do
            hostname <- return $ Prelude.takeWhile (/= ':') hostname
            path <- return $ Prelude.takeWhile (/= '?') (uri request)
            process_get_path addr request hostname path

process_get_path :: String -> Request -> String -> String -> Server (Int, HTTP.Response.Headers, ByteString)
process_get_path addr request hostname path = do
    config <- get_config
    server_name <- return $ config ! "global" ! "server_name"
    if server_name /= hostname then error_response 404 "Document not found"
    else do
        document_root <- find_document_root path
        document <- return $ get_document_path document_root path
        file_contents <- get_file_contents document
        if Data.ByteString.Char8.null file_contents then
            error_response 404 $ "Document not found"
        else do
            ext <- return $ takeExtension document
            return (200,
                    fromList [("Content-Type", get_content_type ext)],
                    file_contents)

find_document_root :: String -> Server (String)
find_document_root path = do
    config <- get_config
    find_document_root_ path (keys config) (config ! "global" ! "document_root", 0)

find_document_root_ :: String -> [String] -> (String, Int) -> Server (String)
find_document_root_ _ [] (document_root, _) = return document_root
find_document_root_ path (section:sections) (document_root, match) = do
    if Data.List.isPrefixOf "path:" section then do
        Just search_path <- return $ stripPrefix "path:" section
        current_match <- return $ Data.List.length search_path
        if Data.List.isPrefixOf search_path path && current_match > match then do
            config <- get_config
            document_root <- return $ config ! section ! "document_root"
            lift $ debug ("Found match for " ++
                          path ++
                          ": " ++
                          search_path ++
                          " -> " ++
                          document_root)

            find_document_root_ path sections (document_root, current_match)
        else
            find_document_root_ path sections (document_root, match)
    else
        find_document_root_ path sections (document_root, match)

get_document_path :: String -> String -> String
get_document_path document_root path = let {
    full_path = joinPath [document_root, dropDrive path]} in
        if hasTrailingPathSeparator full_path then full_path ++ "index.html"
        else full_path

get_file_contents :: String -> Server ByteString
get_file_contents path = liftIO $
    Data.ByteString.Char8.readFile path `catch` ignoreReadException

ignoreReadException :: IOError -> IO ByteString
ignoreReadException error = return Data.ByteString.Char8.empty

ignoreException :: IOError -> IO ()
ignoreException error = return ()

extend_headers :: Int -> HTTP.Response.Headers -> ByteString -> Server (HTTP.Response.Headers)
extend_headers code headers body = do
    config <- get_config
    return $ Data.Map.union headers (fromList [
        ("Connection", "close"),
        ("Content-Length", show $ Data.ByteString.Char8.length body),
        ("Content-Type", "text/plain"),
        ("Server", config ! "global" ! "server_string")
        ])
