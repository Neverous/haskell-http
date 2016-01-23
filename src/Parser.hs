{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Parser where

import Control.Monad (
    ap,
    liftM,
    )

import Control.Monad.Except (
    catchError,
    ExceptT (
        ExceptT
        ),
    MonadError,
    runExceptT,
    throwError,
    )

import Control.Monad.State (
    MonadState,
    State,
    get,
    put,
    runState,
    state,
    )

newtype Parser a = Parser {
    execParser :: ExceptT String (State String) a
    } deriving (Monad, MonadError String, MonadState String)

instance Functor Parser where
    fmap = liftM

instance Applicative Parser where
    pure    = return
    (<*>)   = ap

runParser :: Parser a -> String -> Either String (a, String)
runParser parser str = case (runState . runExceptT . execParser) parser str of
    (Left err, _)   -> Left err
    (Right res, s)  -> Right (res, s)


-- Helpers

eof :: Parser ()
eof = do
    s <- get
    case s of
        []  -> return ()
        x:_ -> throwError ("Unexpected '" ++ [x] ++ "'")

consume_if :: (Char -> Bool) -> Parser Char
consume_if pred = do
    s <- get
    case s of
        x : xs | pred x -> do
            put xs
            return x
        x : _           -> throwError ("Unexpected '" ++ [x] ++ "'")
        []              -> throwError ("Unexpected EOF")

consume :: Char -> Parser Char
consume x = consume_if (== x)

consume_of :: [Char] -> Parser Char
consume_of xs = consume_if (`elem` xs)

repeat :: Parser a -> Parser [a]
repeat parser = recurse `catchError` \_ -> return []
    where recurse = do
            x   <- parser
            xs  <- Parser.repeat parser
            return (x:xs)

consume_many_if :: (Char -> Bool) -> Parser [Char]
consume_many_if pred = Parser.repeat (consume_if pred)

consume_many :: Char -> Parser [Char]
consume_many x = consume_many_if (== x)

consume_many_of :: [Char] -> Parser [Char]
consume_many_of xs = consume_many_if (`elem` xs)
