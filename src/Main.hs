module Main where

import Config (
    Config,
    parse_config
    )

import Control.Monad (
    when
    )

import Data.Map (
    fromList,
    (!)
    )

import Debug (
    choose_verbosity_level,
    info,
    notice,
    string_to_level,
    )

import Network.Socket (
    withSocketsDo
    )

import Options (
    Options (
        Options,
        verbosity,
        config_file
        )
    )

import Server (
    new_server,
    run_server
    )

import System.Console.GetOpt (
    ArgDescr (
        NoArg,
        ReqArg
        ),
    ArgOrder (
        Permute
        ),
    OptDescr,
    OptDescr (
        Option
        ),
    getOpt,
    usageInfo
    )

import System.Environment (
    getArgs,
    getProgName
    )

import System.Exit (
    ExitCode (
        ExitSuccess,
        ExitFailure
        ),
    exitWith
    )

import System.IO (
    hPutStrLn,
    stderr
    )


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
            (ReqArg (\arg opt -> return opt { verbosity = string_to_level arg }) "LEVEL")
            "set verbosity to LEVEL[debug, notice, info, warning, error, silent]",

    Option  ['c']   ["config"]
            (ReqArg (\arg opt -> return opt { config_file = arg }) "FILE")
            "configuration FILE"
    ]

parse_cmdline :: [String] -> IO Options
parse_cmdline argv = case getOpt Permute options argv of
    -- successfully parsed options
    (actions, [], [])  -> do
        opts <- foldl (>>=) (return default_options) actions
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
    options <- return options { verbosity = choose_verbosity_level (verbosity options) (config ! "global" ! "verbosity") }
    notice (verbosity options) "Loaded configuration"
    server <- new_server options config
    run_server server
    info (verbosity options) "Done!"
    return ()
