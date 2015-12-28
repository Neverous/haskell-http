module Main where

import Data.Maybe
import Control.Monad
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

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
main = do
    options <- getArgs >>= parse_cmdline
    putStrLn $ show options
