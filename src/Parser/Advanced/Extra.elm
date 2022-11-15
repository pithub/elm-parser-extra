module Parser.Advanced.Extra exposing
    ( lookAhead, negativeLookAhead
    , followedBy, committed
    , bind, bind2, bind3, bind4, bind5
    , ParserResult, getParserResult, getParserOutcome
    )

{-| Convenience functions for
[`Parser.Advanced`](https://package.elm-lang.org/packages/elm/parser/latest/Parser-Advanced).

---

**Everything here works just like in the
[`Parser.Extra`](Parser-Extra) module, except that `String`
arguments become `Token` arguments, and you need to provide a `Problem` for
certain scenarios.**

---


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

Flipped versions of [`andThen`](https://package.elm-lang.org/packages/elm/parser/latest/Parser-Advanced#andThen)
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

import Parser.Advanced as Parser exposing ((|=), Parser)



-- LOOK AHEAD


{-| Just like [`Parser.Extra.lookAhead`](Parser-Extra#lookAhead).
-}
lookAhead : Parser c x a -> Parser c x a
lookAhead parser =
    bind (getParserResult parser) <|
        \result ->
            case result of
                Ok a ->
                    Parser.succeed a

                Err _ ->
                    Parser.backtrackable parser


{-| Just like [`Parser.Extra.negativeLookAhead`](Parser-Extra#negativeLookAhead)
except you provide a `Token` instead of a `String`.
That way you can specify your custom type of problem
for when the given parser would succeed.
-}
negativeLookAhead : x -> Parser c x a -> Parser c x ()
negativeLookAhead x parser =
    bind (getParserResult parser) <|
        \result ->
            case result of
                Ok _ ->
                    Parser.problem x

                Err _ ->
                    Parser.succeed ()



-- MISCELLANEOUS


{-| Just like [`Parser.Extra.followedBy`](Parser-Extra#followedBy).
-}
followedBy : Parser c x b -> Parser c x a -> Parser c x b
followedBy parser2 parser1 =
    parser1 |> Parser.andThen (\_ -> parser2)


{-| Just like [`Parser.Extra.committed`](Parser-Extra#committed).
-}
committed : Parser c x a -> Parser c x a
committed parser =
    Parser.commit () |> followedBy parser



-- BINDS


{-| Just like [`Parser.Extra.bind`](Parser-Extra#bind).
-}
bind : Parser c x a -> (a -> Parser c x b) -> Parser c x b
bind parser callback =
    parser |> Parser.andThen callback


{-| Just like [`Parser.Extra.bind2`](Parser-Extra#bind2).
-}
bind2 :
    Parser c x a1
    -> Parser c x a2
    -> (a1 -> a2 -> Parser c x b)
    -> Parser c x b
bind2 parser1 parser2 callback =
    bind parser1 <|
        \value1 ->
            bind parser2 <|
                \value2 ->
                    callback value1 value2


{-| Just like [`Parser.Extra.bind3`](Parser-Extra#bind3).
-}
bind3 :
    Parser c x a1
    -> Parser c x a2
    -> Parser c x a3
    -> (a1 -> a2 -> a3 -> Parser c x b)
    -> Parser c x b
bind3 parser1 parser2 parser3 callback =
    bind2 parser1 parser2 <|
        \value1 value2 ->
            bind parser3 <|
                \value3 ->
                    callback value1 value2 value3


{-| Just like [`Parser.Extra.bind4`](Parser-Extra#bind4).
-}
bind4 :
    Parser c x a1
    -> Parser c x a2
    -> Parser c x a3
    -> Parser c x a4
    -> (a1 -> a2 -> a3 -> a4 -> Parser c x b)
    -> Parser c x b
bind4 parser1 parser2 parser3 parser4 callback =
    bind3 parser1 parser2 parser3 <|
        \value1 value2 value3 ->
            bind parser4 <|
                \value4 ->
                    callback value1 value2 value3 value4


{-| Just like [`Parser.Extra.bind5`](Parser-Extra#bind5).
-}
bind5 :
    Parser c x a1
    -> Parser c x a2
    -> Parser c x a3
    -> Parser c x a4
    -> Parser c x a5
    -> (a1 -> a2 -> a3 -> a4 -> a5 -> Parser c x b)
    -> Parser c x b
bind5 parser1 parser2 parser3 parser4 parser5 callback =
    bind4 parser1 parser2 parser3 parser4 <|
        \value1 value2 value3 value4 ->
            bind parser5 <|
                \value5 ->
                    callback value1 value2 value3 value4 value5



-- LOW LEVEL


{-| Result of [`Parser.Advanced.run`](https://package.elm-lang.org/packages/elm/parser/latest/Parser.Advanced#run).
The type alias simplifies the following function signatures a little bit.
-}
type alias ParserResult c x a =
    Result (List (Parser.DeadEnd c x)) a


{-| Just like [`Parser.Extra.getParserResult`](Parser-Extra#getParserResult).
-}
getParserResult : Parser c x a -> Parser c x (ParserResult c x a)
getParserResult parser =
    getParserOutcome parser
        |> Parser.map (Result.map Tuple.first << Tuple.second)


{-| Just like [`Parser.Extra.getParserOutcome`](Parser-Extra#getParserOutcome).
-}
getParserOutcome : Parser c x a -> Parser c x ( String, ParserResult c x ( a, Int ) )
getParserOutcome parser =
    bind2 Parser.getSource Parser.getOffset <|
        \source offset ->
            let
                remainingSource : String
                remainingSource =
                    String.dropLeft offset source

                testParser : Parser c x ( a, Int )
                testParser =
                    Parser.succeed Tuple.pair
                        |= parser
                        |= Parser.getOffset
            in
            Parser.succeed ( remainingSource, Parser.run testParser remainingSource )
