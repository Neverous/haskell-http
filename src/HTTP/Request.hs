module HTTP.Request where

import Data.List
import Data.Map

type Headers = Map String String

data Request =
    Request {
        protocol :: String,
        method :: String,
        uri :: String,
        headers :: Headers
    }

instance Show Request where
    show r  =   ((method r) ++ " " ++ (uri r) ++ " " ++ (protocol r) ++ "\r\n"
            ++  (intercalate "\r\n" (Data.List.map (\(k, v) -> k ++ ": " ++ v)
                                     (toList $ headers r))) ++ "\r\n\r\n")

