# Homework 9: Parsers

**Due**: Monday, April 10 at 10 p.m.

## Background

S-expressions (also known as SExprs) are a simple syntactic format for
tree-structured data, originally developed as syntax for Lisp programs. They are
frequently used by OCaml programmers for serializing data (if you're interested,
[Chapter 20 of the _Real World OCaml_ textbook](https://dev.realworldocaml.org/data-serialization.html)
covers how this works in OCaml).

This week's homework walks you through how to write a simple S-expression
parser, and how to verify the correctness of the parser using QuickCheck.

## Overview of the Parser library

To help you with this HW, we've provided you with a parser library defined in
the file `Parser.hs`. (Please don't modify this file!)

`Parser.hs` contains the following:

-   The definition of a `Parser` type (as discussed in class)
    and relevant typeclass instances
-   A few basic parsers that you will want to use in the exercises

To refer to any parser defined in `Parser.hs` inside `Exercises.hs`,
you should add the `P.` prefix to the function.
For example, if you want to use the `satisfy` parser in `Exercises.hs`,
you would write `P.satisfy` to refer to it.

## Exercise 1 (4 points)

`ident` should parse an S-expression _identifier_, which for our purposes we
define to be a nonempty sequence of alphabetic characters. Fill in `ident`,
making use of `isAlpha :: Char -> Bool`, which is imported for you, and some of
the helper functions provided in `Parser.hs`.

## The Alternative typeclass

Before continuing, read this interlude.

We saw in lecture how the Applicative typeclass by itself can be used to make
parsers for simple, fixed formats. But for any format involving choice (e.g.
“... after the colon there can be a number _or_ a word _or_ parentheses... ”),
we will want to turn to the **Alternative** typeclass, defined as follows:

```haskell
class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
```

`(<|>)` is intended to represent choice: that is, `p1 <|> p2` represents a
choice between `p1` and `p2`. `empty` should be the identity element for
`(<|>)`, and often represents failure.

Note that `Maybe` is an instance of `Alternative`. To see how it works, try these
examples in GHCi:

```haskell
ghci> Just 3 <|> Nothing
ghci> Nothing <|> Just 4
ghci> Just 3 <|> Just 4
ghci> Nothing <|> Nothing
```

We've already implemented a `Parser` instance of `Alternative` for you in
`Parser.hs`. Take a look at the definition there before continuing.

## Exercise 2 (6 points)

An _atom_ is either an integer value (which can be parsed with `P.int`) or an
identifier (which can be parsed with `ident`).

We represent an _atom_ with the type `SExprAtom`, defined as follows:

```haskell
data SExprAtom = Num Int | Ident String
  deriving (Show, Eq)
```

Write a parser `atomParser` that parses `SExprAtom`s. Since there are two
possible constructors, you will want to make use of the `<|>` combinator exposed
by the `Alternative` typeclass!

## Exercise 3 (6 points)

Finally, an S-expression is either an atom, or a list of S-expressions. We
represent this using the type SExpr, defined below:

```haskell
data SExpr
  = Atom SExprAtom
  | Comb [SExpr]
  deriving (Show, Eq)
```

Write a parser `sExprParser` that parses `SExpr`s. We've provided some
scaffolding to ignore whitespace, so you do not need to worry about this.

For atoms, you should make use of `atomParser`.

For lists of S-expressions, they consist of an open parenthesis followed by one
or more S-expressions followed by a close parenthesis. The `parenP` parser shown
in class and provided in `Exercises.hs` will be useful.

## Exercise 4 (4 points, no coding required for this part)

How can we be sure that our parser works as expected?

We can write a _round-trip property_ in QuickCheck! That is, we can make sure
that the result of parsing + pretty-printing a `SExpr` is the same as the
original `SExpr`. To do this, we'll randomly generate many `SExpr`s and check
that the round-trip property holds.

**You don't have to write any code for this exercise** -- just make sure
that your parser satisfies the `prop_roundtrip_sexp` property!

We've already implemented QuickCheck generators & a pretty-printer for `SExprs`
for you, and we've also implemented the round-trip property `prop_roundtrip_sexp`.
These functions are defined at the bottom of `Exercises.hs`. Don't worry
if you don't understand how these functions fully work -- understanding
how these work aren't essential for completing this HW.

You can test this by running `quickCheck prop_roundtrip_sexp` in GHCi,
which will test your parser with 100 randomly generated examples.

If you want to test with more examples, we've provided you with a helper
function `quickCheckN` that can do this. To test your parser with 1000 randomly
generated examples, run `quickCheckN 1000 prop_roundtrip_sexp` in GHCi.

## Grading

Submit only `Exercises.hs` to Gradescope.
This homework will be graded out of 20 points.

## Sources

This assignment is modified from past offerings of CIS 1940 and CIS 5520.

The code for the S-Expression pretty-printer was adapted from the documentation
for the Haskell library
[Text.PrettyPrint.Compact](https://hackage.haskell.org/package/pretty-compact-3.0/docs/Text-PrettyPrint-Compact.html).
