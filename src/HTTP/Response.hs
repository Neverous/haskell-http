module HTTP.Response where

import Data.ByteString
import Data.List
import Data.Map

type Headers = Map String String

data Response =
    Response {
        body :: ByteString,
        code :: Int,
        headers :: Headers,
        protocol :: String
    }

instance Show Response where
    show r =    ((protocol r) ++ " " ++ (show $ code r) ++ " "
            ++  (code_to_string $ code r) ++ "\r\n"
            ++  (Data.List.intercalate "\r\n"
                 (Data.List.map (\(k, v) -> k ++ ": " ++ v)
                 (toList $ headers r))) ++ "\r\n\r\n")

code_to_string :: Int -> String

code_to_string 100 = "Continue"
code_to_string 101 = "Switching Protocols"
code_to_string 103 = "checkpoint"

code_to_string 200 = "OK"
code_to_string 201 = "Created"
code_to_string 202 = "Accepted"
code_to_string 203 = "Non-Authoritative Information"
code_to_string 204 = "No Content"
code_to_string 205 = "Reset Content"
code_to_string 206 = "Partial Content"
code_to_string 226 = "IM Used"

code_to_string 300 = "Multiple Choices"
code_to_string 301 = "Moved Permanently"
code_to_string 302 = "Found"
code_to_string 303 = "See Other"
code_to_string 304 = "Not Modified"
code_to_string 305 = "Use Proxy"
code_to_string 306 = "Switch Proxy"
code_to_string 307 = "Temporary Redirect"
code_to_string 308 = "Permanent Redirect"

code_to_string 400 = "Bad Request"
code_to_string 401 = "Unauthorized"
code_to_string 402 = "Payment Required"
code_to_string 403 = "Forbidden"
code_to_string 404 = "Not Found"
code_to_string 405 = "Method Not Allowed"
code_to_string 406 = "Not Acceptable"
code_to_string 407 = "Proxy Authentication Required"
code_to_string 408 = "Request Timeout"
code_to_string 409 = "Conflict"
code_to_string 410 = "Gone"
code_to_string 411 = "Length Required"
code_to_string 412 = "Precondition Failed"
code_to_string 413 = "Payload Too Large"
code_to_string 414 = "URI Too Long"
code_to_string 415 = "Unsupported Media Type"
code_to_string 416 = "Range Not Satisfiable"
code_to_string 417 = "Expectation Failed"
code_to_string 418 = "I'm a teapot"
code_to_string 419 = "Authentication Timeout"
code_to_string 421 = "Misdirected Request"
code_to_string 426 = "Upgrade Required"
code_to_string 428 = "Precondition Required"
code_to_string 429 = "Too Many Requests"
code_to_string 431 = "Request Header Fields Too Large"
code_to_string 451 = "Unavailable For Legal Reasons"

code_to_string 500 = "Internal Server Error"
code_to_string 501 = "Not Implemented"
code_to_string 502 = "Bad Gateway"
code_to_string 503 = "Service Unavailable"
code_to_string 504 = "Gateway Timeout"
code_to_string 505 = "HTTP Version Not Supported"
code_to_string 506 = "Variant Also Negotiates"
code_to_string 510 = "Not Extended"
code_to_string 511 = "Network Authentication Required"
