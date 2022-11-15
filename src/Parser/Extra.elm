module Parser.Extra exposing
    ( lookAhead, negativeLookAhead
    , followedBy, committed
    , bind, bind2, bind3, bind4, bind5
    , ParserResult, getParserResult, getParserOutcome
    )

{-| Convenience functions for
[`Parser`](https://package.elm-lang.org/packages/elm/parser/latest/Parser).


## Table of Contents

  - [Look Ahead](#look-ahead)
  - [Miscellaneous](#miscellaneous)
  - [Binds](#binds)
  - [Low-Level](#low-level)


# Look Ahead

@docs lookAhead, negativeLookAhead


# Miscellaneous

@docs followedBy, committed


# Binds

Flipped versions of [`andThen`](https://package.elm-lang.org/packages/elm/parser/latest/Parser#andThen)
with 1 to 5 parsers and callbacks with 1 to 5 input parameters.

(In other functional languages, the flipped
version of `andThen` is often called "bind",
hence the names here.)

@docs bind, bind2, bind3, bind4, bind5


# Low-Level

These are low-level functions which are used under the hood to implement
some of the other parsers of this module.

Maybe they can be useful to you, too?

@docs ParserResult, getParserResult, getParserOutcome

-}

import Parser exposing (Parser)
import Parser.Advanced as Advanced
import Parser.Advanced.Extra as AdvancedExtra



-- LOOK AHEAD


{-| A parser that succeeds if the given parser succeeds at the current position
and fails otherwise. The difference to using the original parser is that this
parser doesn't chomp any characters and is always backtrackable.

Lookahead assertions are also known from regular expressions.

In the following example, we create a lookAhead parser from a
[`Parser.token`](https://package.elm-lang.org/packages/elm/parser/latest/Parser#token):

    import Parser exposing (Parser)

    originalParser : Parser ()
    originalParser =
        Parser.token "hello"

    lookAheadParser : Parser ()
    lookAheadParser =
        lookAhead originalParser

Here's how the parsers behave in the success and in the failure case:

    Parser.run originalParser "hello world"
    --> Ok ()

    Parser.run lookAheadParser "hello world"
    --> Ok ()

    Parser.run originalParser "hi world"
    --> Err
    -->   [ { row = 1
    -->     , col = 1
    -->     , problem = Parser.Expecting "hello"
    -->     }
    -->   ]

    Parser.run lookAheadParser "hi world"
    --> Err
    -->   [ { row = 1
    -->     , col = 1
    -->     , problem = Parser.Expecting "hello"
    -->     }
    -->   ]

If the original parser succeeds, the lookAhead parser succeeds too,
and if the original parser fails, so does the lookAhead parser.

It seems that both are doing the same.
The difference is that the lookAhead parser doesn't chomp any characters:

    Parser.run (Parser.getChompedString originalParser)
        "hello world"
    --> Ok "hello"

    Parser.run (Parser.getChompedString lookAheadParser)
        "hello world"
    --> Ok ""

-}
lookAhead : Parser a -> Parser a
lookAhead =
    AdvancedExtra.lookAhead


{-| A parser that succeeds if the given parser fails at the current position
and fails with the given problem message otherwise.
This parser doesn't chomp any characters and is always backtrackable.

Negative lookahead assertions are also known from regular expressions.

In the following example, we create a negativeLookAhead parser from a
[`Parser.int`](https://package.elm-lang.org/packages/elm/parser/latest/Parser#int):

    import Parser exposing (Parser)

    originalParser : Parser Int
    originalParser =
        Parser.int

    negativeLookAheadParser : Parser ()
    negativeLookAheadParser =
        negativeLookAhead "unexpected int" originalParser

Here's how the parsers behave in the success and in the failure case:

    Parser.run originalParser "123abc"
    --> Ok 123

    Parser.run negativeLookAheadParser "123abc"
    --> Err
    -->   [ { row = 1
    -->     , col = 1
    -->     , problem = Parser.Problem "unexpected int"
    -->     }
    -->   ]

    Parser.run originalParser "abc123"
    --> Err
    -->   [ { row = 1
    -->     , col = 1
    -->     , problem = Parser.ExpectingInt
    -->     }
    -->   ]

    Parser.run negativeLookAheadParser "abc123"
    --> Ok ()

If the original parser succeeds, the negativeLookAhead parser fails,
and if the original parser fails, the negativeLookAhead parser succeeds.

-}
negativeLookAhead : String -> Parser a -> Parser ()
negativeLookAhead msg =
    AdvancedExtra.negativeLookAhead (Parser.Problem msg)



-- MISCELLANEOUS


{-| Executes two parsers in sequence, discarding the first result.

Note that the arguments are flipped to make the function easily
usable with the forward pipe operator:

    import Parser exposing (Parser)

    testParser : Parser Int
    testParser =
        Parser.token "> "
            |> followedBy Parser.int

    Parser.run testParser "> 123"
    --> Ok 123

-}
followedBy : Parser b -> Parser a -> Parser b
followedBy =
    AdvancedExtra.followedBy


{-| The opposite of [`backtrackable`](https://package.elm-lang.org/packages/elm/parser/latest/Parser#backtrackable).
The parser is made _not_ backtrackable, a.k.a. "committed".

It's implemented as:

    committed : Parser a -> Parser a
    committed parser =
        Parser.commit ()
            |> followedBy parser

-}
committed : Parser a -> Parser a
committed =
    AdvancedExtra.committed



-- BINDS


{-| Flipped version of [`andThen`](https://package.elm-lang.org/packages/elm/parser/latest/Parser#andThen).

If you use `andThen` without the forward pipe operator,
this can save some parentheses and an extra level of indenting.

    andThen:

        parser
            |> andThen
                (\value ->
                    ...
                )

    bind:

        bind parser <|
            \value ->
                ...

-}
bind : Parser a -> (a -> Parser b) -> Parser b
bind =
    AdvancedExtra.bind


{-| Runs the 2 parsers one after the other and then calls the callback
function with the 2 parsed values.

Stops at the first failure.

This can be handy if you need to independently parse 2 values and then
process them together. It reduces the indentation of the code:

    andThen:

        getSource
            |> andThen
                (\source ->
                    getOffset
                        |> andThen
                            (\offset ->
                                ...
                            )
                )

    bind:

        bind getSource <|
            \source ->
                bind getOffset <|
                    \offset ->
                        ...

    bind2:

        bind2 getSource getOffset <|
            \source offset ->
                ...

-}
bind2 :
    Parser a1
    -> Parser a2
    -> (a1 -> a2 -> Parser b)
    -> Parser b
bind2 =
    AdvancedExtra.bind2


{-| Runs the 3 parsers one after the other and then calls the callback
function with the 3 parsed values.

Stops at the first failure.

This can be handy if you need to independently parse 3 values and then
process them together. It reduces the indentation of the code.
For an example with 2 parsers see [`bind2`](#bind2).

-}
bind3 :
    Parser a1
    -> Parser a2
    -> Parser a3
    -> (a1 -> a2 -> a3 -> Parser b)
    -> Parser b
bind3 =
    AdvancedExtra.bind3


{-| Runs the 4 parsers one after the other and then calls the callback
function with the 4 parsed values.

Stops at the first failure.

This can be handy if you need to independently parse 4 values and then
process them together. It reduces the indentation of the code.
For an example with 2 parsers see [`bind2`](#bind2).

-}
bind4 :
    Parser a1
    -> Parser a2
    -> Parser a3
    -> Parser a4
    -> (a1 -> a2 -> a3 -> a4 -> Parser b)
    -> Parser b
bind4 =
    AdvancedExtra.bind4


{-| Runs the 5 parsers one after the other and then calls the callback
function with the 5 parsed values.

Stops at the first failure.

This can be handy if you need to independently parse 5 values and then
process them together. It reduces the indentation of the code.
For an example with 2 parsers see [`bind2`](#bind2).

-}
bind5 :
    Parser a1
    -> Parser a2
    -> Parser a3
    -> Parser a4
    -> Parser a5
    -> (a1 -> a2 -> a3 -> a4 -> a5 -> Parser b)
    -> Parser b
bind5 =
    AdvancedExtra.bind5



-- LOW LEVEL


{-| Result of [`Parser.run`](https://package.elm-lang.org/packages/elm/parser/latest/Parser#run).
The type alias simplifies the following function signatures a little bit.
-}
type alias ParserResult a =
    Result (List Parser.DeadEnd) a


{-| Gets the result of running the given parser at the current position.

**Note:** the test run has absolutely **no effect** on the current parser state!

This could be used to implement a [`lookAhead`](#lookAhead) parser on your own
(in fact, this _is_ the real implementation):

    import Parser exposing ((|=), Parser)

    myLookAhead : Parser a -> Parser a
    myLookAhead parser =
        bind (getParserResult parser) <|
            \result ->
                case result of
                    Ok a ->
                        Parser.succeed a
                    Err _ ->
                        -- here we know that the parser
                        -- fails with the expected error
                        -- but we want it backtrackable
                        Parser.backtrackable parser

    lookAheadParser : Parser Int
    lookAheadParser =
        myLookAhead Parser.int

    sampleParser : Parser ( Int, String )
    sampleParser =
        Parser.succeed Tuple.pair
            |= lookAheadParser
            |= Parser.getChompedString lookAheadParser

    Parser.run sampleParser "123abc"
    --> Ok ( 123, "" )

The `sampleParser` calls the `lookAheadParser` twice:

  - once to get the parsed value
  - once to get the chomped string

As expected, the parsed value is returned _without_ chomping any characters.

-}
getParserResult : Parser a -> Parser (ParserResult a)
getParserResult parser =
    AdvancedExtra.getParserResult parser
        |> Parser.map (Result.mapError (List.map problemToDeadEnd))


{-| An enhanced version of [`getParserResult`](#getParserResult).
It additionally gets:

  - the remaining source string (the source string starting at the current parser position)
  - the offset where the given parser stopped, relative to the beginning of the
    remaining source string (if the given parser succeeded)

The following example shows all these values:

    import Parser exposing ((|.), (|=), Parser)

    originalParser : Parser Int
    originalParser =
        Parser.int

    outcomeParser : Parser ( String, ParserResult ( Int, Int ) )
    outcomeParser =
        getParserOutcome originalParser

The `outcomeParser` performs a test run of the `originalParser`
and then yields the results of the test run as described above.

    sampleParser : Parser ( String, ParserResult ( Int, Int ) )
    sampleParser =
        Parser.token "hello "
            |> followedBy outcomeParser

The `sampleParser` first chomps the string "hello "
(so that the current parser position is not at the beginning)
and then calls the `outcomeParser`
(using this package's function [`followedBy`](#followedBy)).

The results when the `originalParser` succeeds:

    Parser.run sampleParser "hello 123 world"
    --> Ok ( "123 world", Ok ( 123, 3 ))

  - the remaining source string after the initial "hello " is "123 world"
  - the result of the `originalParser` is 123
  - the position after the `originalParser` is run is 3
    (relative to the beginning of the remaining source string)
    which means that the `originalParser` has chomped 3 characters

The results when the `originalParser` fails:

    Parser.run sampleParser "hello world"
    --> Ok
    -->   ( "world"
    -->   , Err
    -->       [ { row = 1
    -->         , col = 1
    -->         , problem = Parser.ExpectingInt
    -->         }
    -->       ]
    -->   )

  - the remaining source string after the initial "hello " is "world"
  - the error of the `originalParser`

Note that the error position is relative to the beginning of the remaining source string, too.

-}
getParserOutcome : Parser a -> Parser ( String, ParserResult ( a, Int ) )
getParserOutcome parser =
    AdvancedExtra.getParserOutcome parser
        |> Parser.map (Tuple.mapSecond (Result.mapError (List.map problemToDeadEnd)))



-- UTILS


problemToDeadEnd : Advanced.DeadEnd Never Parser.Problem -> Parser.DeadEnd
problemToDeadEnd p =
    Parser.DeadEnd p.row p.col p.problem
