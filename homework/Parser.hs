{-# LANGUAGE InstanceSigs #-}

module Parser
  ( Parser,
    doParse,
    satisfy,
    zeroOrMore,
    oneOrMore,
    char,
    int,
  )
where

import Control.Applicative
import Control.Monad
import Data.Char
import Prelude hiding (filter)

-- Don't modify this file!

-- Definition of the parser type
-- The `P` data constructor is not exported by this library
newtype Parser a = P {doParse :: String -> Maybe (a, String)}

-- Functor instance for Parser
instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = P $ \s -> do
    (c, cs) <- doParse p s
    return (f c, cs)

-- Applicative instance for Parser
instance Applicative Parser where
  pure :: a -> Parser a
  pure a = P (\s -> Just (a, s))

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  p1 <*> p2 = P $ \s -> do
    (f, s') <- doParse p1 s
    (x, s'') <- doParse p2 s'
    return (f x, s'')

-- Alternative instance for Parser
instance Alternative Parser where
  -- Always fail
  empty :: Parser a
  empty = P $ const Nothing

  -- Try the left parser, if that fails then try the right
  (<|>) :: Parser a -> Parser a -> Parser a
  p1 <|> p2 = P $ \s -> doParse p1 s <|> doParse p2 s

------------------------------------------------------------------------
-- SOME USEFUL PARSERS

-- | Parser takes a Char predicate and parses it
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = P doParse
  where
    doParse [] = Nothing
    doParse (x : xs)
      | p x = Just (x, xs)
      | otherwise = Nothing

-- Take a parser and make it parse zero or more occurrences
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

-- Take a parser and make it parse one or more occurrences
oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

-- Parser that parses one specific character c (fails otherwise)
char :: Char -> Parser Char
char c = satisfy (== c)

-- Parser that parses a (positive or negative) integer (fails otherwise)
int :: Parser Int
int = read <$> ((:) <$> char '-' <*> some digit <|> some digit)
  where
    digit = satisfy isDigit