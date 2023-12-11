{- HLINT ignore "Use lambda-case" -}

module Exercises where

import Control.Applicative
import Data.Char (isDigit)
import Parser
import Test.HUnit
import Text.Read (readMaybe)

-- Exercise 1: Write a parser that parses one single digit

oneDigit :: Parser Int
oneDigit = undefined "TODO"

-- Test cases for `oneDigit`
-- Test by typing `runTestTT test_oneDigit` into GHCi
test_oneDigit :: Test
test_oneDigit =
  TestList
    [ doParse oneDigit "3" ~?= Just (3, ""),
      doParse oneDigit "0" ~?= Just (0, ""),
      -- Only parses the first digit
      doParse oneDigit "915" ~?= Just (9, "15"),
      -- can't parse non-digits
      doParse oneDigit "notAnInt" ~?= Nothing
    ]

----------------------------------------------------------------------------------------------------------------
-- Exercise 2: Write a parser that takes an existing parser, and parses between parentheses
parenP :: Parser a -> Parser a
parenP p = undefined "TODO"

-- Here are some helper parsers for you to test parenP (there are more in `Parser.hs`)
-- The parser `get` parses one single char

get :: Parser Char
get = P $ \s ->
  case s of
    (c : cs) -> Just (c, cs)
    [] -> Nothing

-- Test cases for parenP & `get`
-- Test by typing `runTestTT test_parenPGet` into GHCi
test_parenPGet :: Test
test_parenPGet =
  TestList
    [ doParse (parenP get) "(c)" ~?= Just ('c', ""),
      doParse (parenP get) "(x)" ~?= Just ('x', ""),
      doParse (parenP get) "(k)some more text" ~?= Just ('k', "some more text")
    ]

-- The parser `int` succeeds only if the input is a (positive or negative) integer
int :: Parser Int
int = read <$> ((:) <$> char '-' <*> some digit <|> some digit)
  where
    digit = satisfy isDigit

-- Test cases for parenP & `int`
-- Test by typing `runTestTT test_parenPGet` into GHCi
test_parenPInt :: Test
test_parenPInt =
  TestList
    [ doParse (parenP int) "(-34)" ~?= Just (-34, ""),
      doParse (parenP int) "(149)" ~?= Just (149, ""),
      doParse (parenP int) "(0)" ~?= Just (0, ""),
      doParse (parenP int) "(notAnInt)" ~?= Nothing,
      doParse (parenP int) "(1)someMoreText" ~?= Just (1, "someMoreText")
    ]
