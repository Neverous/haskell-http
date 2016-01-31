module Options where

data Options =
    Options {
        verbosity   :: Int,
        config_file :: String
    }
    deriving Show
