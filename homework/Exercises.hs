{-# LANGUAGE InstanceSigs #-}

module Exercises where

import Control.Applicative
import Data.Char
import Data.Functor
import Parser (Parser, doParse)
-- To refer to anything defined in `Parser.hs`, add the `P.` prefix before it
import qualified Parser as P
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck (Arbitrary (..), Gen, quickCheck, withMaxSuccess)
import qualified Test.QuickCheck as QC
import Text.PrettyPrint (Doc, (<>))
import qualified Text.PrettyPrint as PP
import Prelude hiding ((<>))

{- Read [instructions.md] first. -}

------------------------------------------------------------
--  1. Parsing S-expression identifiers.
------------------------------------------------------------

ident :: Parser String
ident = undefined "TODO"

-- Check by typing `runTestTT test_ident` into GHCi
test_ident :: Test
test_ident =
  TestList
    [ doParse ident "foo bar" ~?= Just ("foo", " bar"),
      doParse ident "13" ~?= Nothing, -- identifiers can't be numbers
      doParse ident " " ~?= Nothing,
      doParse ident "" ~?= Nothing
    ]

------------------------------------------------------------
--  2. Parsing S-expression atoms.
------------------------------------------------------------

-- An "atom" is either an integer value or an "identifier".
data SExprAtom = Num Int | Ident String
  deriving (Show, Eq)

atomParser :: Parser SExprAtom
atomParser = undefined "TODO"

-- Check by typing `runTestTT test_atomParser` into GHCi
test_atomParser :: Test
test_atomParser =
  TestList
    [ doParse atomParser "5" ~?= Just (Num 5, ""),
      doParse atomParser "ident" ~?= Just (Ident "ident", "")
    ]

------------------------------------------------------------
--  3. Parsing S-expressions.
------------------------------------------------------------

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr
  = Atom SExprAtom
  | Comb [SExpr]
  deriving (Show, Eq)

sExprParser :: Parser SExpr
sExprParser = spacesP (undefined "TODO")

-- Takes a parser and parses between parentheses
parenP :: Parser a -> Parser a
parenP p = P.char '(' *> p <* P.char ')'

-- Takes a parser and ignores leading and trailing whitespace
spacesP :: Parser a -> Parser a
spacesP p = spaces *> p <* spaces
  where
    spaces :: Parser String
    spaces = P.zeroOrMore (P.satisfy isSpace)

-- Check by typing `runTestTT test_SExprParser` into GHCi
test_SExprParser :: Test
test_SExprParser =
  TestList
    [ doParse sExprParser "" ~?= Nothing,
      doParse sExprParser "5" ~?= Just (Atom (Num 5), ""),
      doParse sExprParser "(bar (foo) 3 5 874)"
        ~?= Just
          ( Comb
              [ Atom (Ident "bar"),
                Comb [Atom (Ident "foo")],
                Atom (Num 3),
                Atom (Num 5),
                Atom (Num 874)
              ],
            ""
          )
    ]

------------------------------------------------------------
--  4. Testing our S-Expression parser using QuickCheck
------------------------------------------------------------
-- See `instructions.md` for details on this section -- you don't
-- need to write any code for this exercise! You just need to check
-- that your parser passes the QuickCheck property below.

-- Try running this QuickCheck property in GHCi!
-- Type `quickCheck prop_roundtrip_sexp` in GHCi
prop_roundtrip_sexp :: SExpr -> Bool
prop_roundtrip_sexp sexpr = doParse sExprParser (printSExpr sexpr) == Just (sexpr, "")

-- Helper QuickCheck function for running lots of tests while making sure
-- our randomly-generated S-Expressions don't become too big
-- Usage: to run 10000 tests, type `quickCheckN 10000` in GHCi
quickCheckN :: QC.Testable prop => Int -> prop -> IO ()
quickCheckN n = QC.quickCheckWith $ QC.stdArgs {QC.maxSuccess = n, QC.maxSize = 100}

-- Pretty-printer for S-Expressions
-- (You do not need to understand how this function is implemented!)
printSExpr :: SExpr -> String
printSExpr = PP.render . pretty
  where
    pretty :: SExpr -> Doc
    pretty (Atom (Num n)) = PP.int n
    pretty (Atom (Ident ident)) = PP.text ident
    pretty (Comb xs) = PP.text "(" <> PP.sep (map pretty xs) <> PP.text ")"

-- QuickCheck generators for S-Expressions
-- (You do not need to understand how these generators are implemented!)

-- | Generator of Idents
-- Specifically, this generator produces random non-empty strings up to length 5 only containing
-- alphabetic characters
genIdent :: Gen String
genIdent = QC.resize 5 $ QC.listOf1 (QC.elements ['a' .. 'z'])

-- Generator of Atoms
genAtom :: Gen SExprAtom
genAtom =
  QC.oneof
    [ Num <$> arbitrary,
      Ident <$> genIdent
    ]

-- Generator of S-Expressions
instance Arbitrary SExpr where
  arbitrary :: Gen SExpr
  arbitrary =
    QC.frequency
      [ (2, Atom <$> genAtom),
        (1, Comb <$> genComb 3)
      ]
    where
      genComb n = QC.resize (n `div` 2) (QC.listOf1 arbitrary)

---- end of exercises ----

{- Write down the approximate number of hours
it took you to complete this homework. If you have any
comments, feel free to also write them here. -}

time :: Double
time = undefined "TODO"
