module HTTP.Parser where

import Data.Map
import HTTP.Request
import Parser

eol :: String
eol = "\r\n"

whitespaces :: String
whitespaces = " \t\n\r"

consume_eol :: Parser String
consume_eol = consume_s eol

method_parser :: Parser (String, String, String) -- method, uri, protocol
method_parser = do
    method      <- consume_many_if (`notElem` whitespaces)
    consume_many ' '
    uri         <- consume_many_if (`notElem` whitespaces)
    consume_many ' '
    protocol    <- consume_many_if (`notElem` whitespaces)
    consume_eol
    return (method, uri, protocol)

header_parser :: Parser (String, String) -- key -> value
header_parser = do
    key <- consume_many_if (`notElem` (":" ++ whitespaces))
    consume ':'
    consume_many ' '
    value <- consume_many_if (/= '\r')
    consume_eol
    return (key, value)

request_parser :: Parser Request
request_parser = do
    (method, uri, protocol) <- method_parser
    headers <- Parser.repeat header_parser
    consume_eol
    return Request {
        method = method,
        uri = uri,
        protocol = protocol,
        headers = (fromList headers)
    }

parse_request :: String -> Either String Request
parse_request input = case runParser request_parser input of
    (Left errs)         -> Left errs
    (Right (config, _)) -> Right config
