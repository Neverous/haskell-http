module Config where

import Data.Map (
    Map,
    fromList,
    union,
    unionWith,
    )

import Parser (
    Parser,
    consume,
    consume_many_if,
    consume_many_of,
    eof,
    repeat,
    runParser,
    )

whitespaces :: String
whitespaces = " \t\n\r"

type Section = Map String String
type Config = Map String (Section)

skip_whitespace :: Parser ()
skip_whitespace = do
    consume_many_of whitespaces
    return ()

skip_comments :: Parser ()
skip_comments = do
    Parser.repeat comment_parser
    return ()
    where comment_parser = do
            skip_whitespace
            consume ';'
            consume_many_if (/= '\n')
            return ()

option_parser :: Parser (String, String)
option_parser = do
    skip_comments
    skip_whitespace
    key <- consume_many_if (`notElem` (whitespaces ++ "=[]"))
    skip_whitespace
    consume '='
    skip_whitespace
    value <- consume_many_if (/= '\n')
    return (key, value)

section_parser :: Parser (String, Section)
section_parser = do
    skip_comments
    skip_whitespace
    consume '['
    skip_whitespace
    name <- consume_many_if (/= ']')
    skip_whitespace
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
