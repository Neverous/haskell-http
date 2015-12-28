module Config where

import Data.Map (
    Map
    )

type Config = Map String (Map String String)

load_configuration :: String -> Config -> IO Config
load_configuration config_file config = do
    --config <- parse_file config_file config
    return config

