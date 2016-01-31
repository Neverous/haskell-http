module Main where

import Config
import Control.Monad.Trans
import Data.Map
import Debug
import Network.Socket
import Options
import Server
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

default_options :: Options
default_options = Options {
    verbosity   = 0,
    config_file = "config.ini"
}

default_config :: Config
default_config = fromList [
        ("global", fromList [
            ("host", "0.0.0.0"),
            ("port", "80"),
            ("server_name", "hhttp.com"),
            ("server_string", "hhttp 0.1"),
            ("root_path", "/srv/www/"),
            ("workers_count", "1"),
            ("verbosity", "0"),
            ("config_file", "config.ini")
        ])
    ]

options :: [OptDescr (Options -> IO Options)]
options = [
    Option  ['h']   ["help"]
            (NoArg (\_ -> do
                        program <- getProgName
                        hPutStrLn stderr (usageInfo program options)
                        exitWith ExitSuccess))
            "print this help message",

    Option  ['v']   ["verbosity"]
            (ReqArg (\arg opt -> return opt { verbosity = string_to_level arg })
                "LEVEL")
            "set verbosity to LEVEL[debug, notice, info, warning, error, silent]",

    Option  ['c']   ["config"]
            (ReqArg (\arg opt -> return opt { config_file = arg }) "FILE")
            "configuration FILE"
    ]

parse_cmdline :: [String] -> IO Options
parse_cmdline argv = case getOpt Permute options argv of
    -- successfully parsed options
    (actions, [], [])  -> do
        opts <- Prelude.foldl (>>=) (return default_options) actions
        return opts

    -- some arguments left
    (_, _, [])      -> do
        program <- getProgName
        hPutStrLn stderr (  "This program requires no arguments\n" ++
                            usageInfo program options)
        exitWith (ExitFailure 2)

    -- error while parsing
    (_, _, errs)    -> do
        program <- getProgName
        hPutStrLn stderr (concat errs ++ usageInfo program options)
        exitWith (ExitFailure 1)

load_configuration :: String -> Config -> IO Config
load_configuration config_file config = do
    input <- readFile config_file
    case parse_config input config of
        -- sucessfully parsed config
        Right config -> return config
        -- errors
        Left err -> do
            hPutStrLn stderr (  "Couldn't load configuration file!\n" ++
                                err ++
                                "\n")
            exitWith (ExitFailure 3)

main :: IO ()
main = withSocketsDo $ do
    args    <- getArgs
    options <- parse_cmdline args
    config  <- load_configuration (config_file options) default_config
    verbosity <- return $
        let {level = verbosity options} in
            if level > 0 then level
            else read (config ! "global" ! "verbosity") :: Int

    runDebugIO verbosity $ do
        notice "Loaded configuration"
        runServer config
        info "Done!"
        return ()
