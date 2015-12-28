module Main where

import Config (
    Config,
    load_configuration
    )

import Control.Monad (
    when
    )

import Data.Map (
    fromList
    )

import Network.Socket (
    withSocketsDo
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


data Options =
    Options {
        verbose     :: Bool,
        config_file :: String
    }
    deriving Show

default_options :: Options
default_options = Options {
    verbose     = False,
    config_file = "config.ini"
}

default_config :: Config
default_config = fromList [
        ("global", fromList [
            ("server_string", "hhttp 0.1"),
            ("root_path", "/srv/www/"),
            ("workers_count", "1"),
            ("verbose", "0"),
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

    Option  ['v']   ["verbose"]
            (NoArg (\opt -> return opt { verbose = True }))
            "server verbose output",

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

main :: IO ()
main = withSocketsDo $ do
    options <- getArgs >>= parse_cmdline
    config <- load_configuration (config_file options) default_config
    -- debug config "Loaded configuration"
    -- server <- new_server config
    -- run_server server
    putStrLn $ show config
    return ()
