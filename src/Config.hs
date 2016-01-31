module Config where

import Data.Map
import Parser

type Section = Map String String
type Config = Map String (Section)

whitespaces :: String
whitespaces = " \t\n\r"

skip_whitespaces :: Parser ()
skip_whitespaces = do
    consume_many_of whitespaces
    return ()

skip_comments :: Parser ()
skip_comments = do
    Parser.repeat comment_parser
    return ()
    where comment_parser = do
            skip_whitespaces
            consume ';'
            consume_many_if (/= '\n')
            return ()

option_parser :: Parser (String, String)
option_parser = do
    skip_comments
    skip_whitespaces
    key <- consume_many_if (`notElem` (whitespaces ++ "=[]"))
    skip_whitespaces
    consume '='
    skip_whitespaces
    value <- consume_many_if (/= '\n')
    return (key, value)

section_parser :: Parser (String, Section)
section_parser = do
    skip_comments
    skip_whitespaces
    consume '['
    skip_whitespaces
    name <- consume_many_if (/= ']')
    skip_whitespaces
    consume ']'
    options <- Parser.repeat option_parser
    return (name, fromList options)

config_parser :: Config -> Parser Config
config_parser config = do
    sections <- Parser.repeat section_parser
    eof
    return (unionWith (union) (fromList sections) config)

parse_config :: String -> Config -> Either String Config
parse_config input config = case runParser (config_parser config) input of
    (Left errs)         -> Left errs
    (Right (config, _)) -> Right config
