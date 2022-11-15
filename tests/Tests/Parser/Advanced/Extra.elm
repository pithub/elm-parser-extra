module Tests.Parser.Advanced.Extra exposing (suite)

import Expect
import Parser.Advanced as Parser exposing ((|.), (|=), Parser)
import Parser.Advanced.Extra as ParserExtra
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Parser.Advanced.Extra"
        [ Test.describe "internal tools" internalToolsTests
        , Test.describe "lookAhead" lookAheadTests
        , Test.describe "negativeLookAhead" negativeLookAheadTests
        , Test.describe "followedBy" followedByTests
        , Test.describe "committed" committedTests
        , Test.describe "binds" bindsTests
        , Test.describe "getParserResult" getParserResultTests
        , Test.describe "getParserOutcome" getParserOutcomeTests
        ]


lookAheadTests : List Test
lookAheadTests =
    let
        parser : Parser c String Int
        parser =
            ParserExtra.lookAhead <|
                Parser.int "expecting int" "invalid int"
    in
    [ Test.test "positive -> no error, same result" <|
        \() ->
            Parser.run
                parser
                "123 rest"
                |> Expect.equal
                    (Ok 123)
    , Test.test "positive -> no error, position at start" <|
        \() ->
            Parser.run
                (parser
                    |> Parser.andThen (\_ -> Parser.getOffset)
                )
                "123 rest"
                |> Expect.equal
                    (Ok 0)
    , Test.test "positive -> no error, backtrackable" <|
        \() ->
            Parser.run
                (parser |> successBacktrackableCheck)
                "123 rest"
                |> Expect.equal
                    (Ok "backtrackable")
    , Test.test "negative -> normal error" <|
        \() ->
            Parser.run
                parser
                ". rest"
                |> Expect.equal
                    (Err
                        [ { row = 1
                          , col = 1
                          , problem = "invalid int"
                          , contextStack = []
                          }
                        ]
                    )
    , Test.test "negative -> backtrackable" <|
        \() ->
            Parser.run
                (parser |> failureBacktrackableCheck)
                ". rest"
                |> Expect.equal
                    (Ok "backtrackable")
    ]


negativeLookAheadTests : List Test
negativeLookAheadTests =
    let
        parser : Parser c String ()
        parser =
            ParserExtra.negativeLookAhead "not found" <|
                Parser.int "expecting int" "invalid int"
    in
    [ Test.test "positive -> error" <|
        \() ->
            Parser.run
                parser
                "123 rest"
                |> Expect.equal
                    (Err
                        [ { row = 1
                          , col = 1
                          , problem = "not found"
                          , contextStack = []
                          }
                        ]
                    )
    , Test.test "positive -> backtrackable" <|
        \() ->
            Parser.run
                (parser |> failureBacktrackableCheck)
                "123 rest"
                |> Expect.equal
                    (Ok "backtrackable")
    , Test.test "negative -> no error, position at start" <|
        \() ->
            Parser.run
                (parser
                    |> Parser.andThen (\() -> Parser.getOffset)
                )
                ". rest"
                |> Expect.equal
                    (Ok 0)
    , Test.test "negative -> no error, backtrackable" <|
        \() ->
            Parser.run
                (parser |> successBacktrackableCheck)
                ". rest"
                |> Expect.equal
                    (Ok "backtrackable")
    ]


followedByTests : List Test
followedByTests =
    let
        parser : Parser c String ()
        parser =
            Parser.int "expecting int" "invalid int"
                |> ParserExtra.followedBy
                    (Parser.token (Parser.Token "-" "expecting -"))
    in
    [ Test.test "followedBy no error" <|
        \() ->
            Parser.run
                parser
                "123-end"
                |> Expect.equal
                    (Ok ())
    , Test.test "followedBy error at second parser" <|
        \() ->
            Parser.run
                parser
                "123xend"
                |> Expect.equal
                    (Err
                        [ { row = 1
                          , col = 4
                          , problem = "expecting -"
                          , contextStack = []
                          }
                        ]
                    )
    , Test.test "followedBy error at first parser" <|
        \() ->
            Parser.run
                parser
                "x-end"
                |> Expect.equal
                    (Err
                        [ { row = 1
                          , col = 1
                          , problem = "expecting int"
                          , contextStack = []
                          }
                        ]
                    )
    ]


committedTests : List Test
committedTests =
    [ Test.test "backtrackable no error -> no error" <|
        \() ->
            Parser.run
                (Parser.succeed 123
                    |> ParserExtra.committed
                )
                ""
                |> Expect.equal
                    (Ok 123)
    , Test.test "backtrackable no error -> committed" <|
        \() ->
            Parser.run
                (Parser.succeed 123
                    |> ParserExtra.committed
                    |> successBacktrackableCheck
                )
                ""
                |> Expect.equal
                    (Ok "committed")
    , Test.test "backtrackable error -> error" <|
        \() ->
            Parser.run
                (Parser.problem "error"
                    |> ParserExtra.committed
                )
                ""
                |> Expect.equal
                    (Err
                        [ { row = 1
                          , col = 1
                          , problem = "error"
                          , contextStack = []
                          }
                        ]
                    )
    , Test.test "backtrackable error -> committed" <|
        \() ->
            Parser.run
                (Parser.problem "error"
                    |> ParserExtra.committed
                    |> failureBacktrackableCheck
                )
                ""
                |> Expect.equal
                    (Ok "committed")
    , Test.test "committed no error -> no error" <|
        \() ->
            Parser.run
                (Parser.int "expecting int" "invalid int"
                    |> ParserExtra.committed
                )
                "123"
                |> Expect.equal
                    (Ok 123)
    , Test.test "committed no error -> committed" <|
        \() ->
            Parser.run
                (Parser.int "expecting int" "invalid int"
                    |> ParserExtra.committed
                    |> successBacktrackableCheck
                )
                "123"
                |> Expect.equal
                    (Ok "committed")
    , Test.test "committed error -> error" <|
        \() ->
            Parser.run
                (Parser.int "expecting int" "invalid int"
                    |> ParserExtra.committed
                )
                "."
                |> Expect.equal
                    (Err
                        [ { row = 1
                          , col = 1
                          , problem = "invalid int"
                          , contextStack = []
                          }
                        ]
                    )
    , Test.test "committed error -> committed" <|
        \() ->
            Parser.run
                (Parser.int "expecting int" "invalid int"
                    |> ParserExtra.committed
                    |> failureBacktrackableCheck
                )
                "."
                |> Expect.equal
                    (Ok "committed")
    ]


bindsTests : List Test
bindsTests =
    [ Test.test "bind no error" <|
        \() ->
            Parser.run
                (ParserExtra.bind
                    (Parser.int "expecting int" "invalid int")
                    (\i -> Parser.succeed i)
                )
                "123|end"
                |> Expect.equal
                    (Ok 123)
    , Test.test "bind2 no error" <|
        \() ->
            Parser.run
                (ParserExtra.bind2
                    (Parser.int "expecting int" "invalid int")
                    (Parser.token (Parser.Token "-a-" "expecting -a-"))
                    (\i () -> Parser.succeed i)
                )
                "123-a-|end"
                |> Expect.equal
                    (Ok 123)
    , Test.test "bind3 no error" <|
        \() ->
            Parser.run
                (ParserExtra.bind3
                    (Parser.int "expecting int 1" "invalid int 1")
                    (Parser.token (Parser.Token "-a-" "expecting -a-"))
                    (Parser.int "expecting int 2" "invalid int 2")
                    (\i1 () i2 -> Parser.succeed ( i1, i2 ))
                )
                "123-a-456|end"
                |> Expect.equal
                    (Ok ( 123, 456 ))
    , Test.test "bind4 no error" <|
        \() ->
            Parser.run
                (ParserExtra.bind4
                    (Parser.int "expecting int 1" "invalid int 1")
                    (Parser.token (Parser.Token "-a-" "expecting -a-"))
                    (Parser.int "expecting int 2" "invalid int 2")
                    (Parser.token (Parser.Token "-b-" "expecting -b-"))
                    (\i1 () i2 () -> Parser.succeed ( i1, i2 ))
                )
                "123-a-456-b-|end"
                |> Expect.equal
                    (Ok ( 123, 456 ))
    , Test.test "bind5 no error" <|
        \() ->
            Parser.run
                (ParserExtra.bind5
                    (Parser.int "expecting int 1" "invalid int 1")
                    (Parser.token (Parser.Token "-a-" "expecting -a-"))
                    (Parser.int "expecting int 2" "invalid int 2")
                    (Parser.token (Parser.Token "-b-" "expecting -b-"))
                    (Parser.int "expecting int 3" "invalid int 3")
                    (\i1 () i2 () i3 -> Parser.succeed ( i1, i2, i3 ))
                )
                "123-a-456-b-789|end"
                |> Expect.equal
                    (Ok ( 123, 456, 789 ))
    , Test.test "bind5 error in 5 of 5" <|
        \() ->
            Parser.run
                (ParserExtra.bind5
                    (Parser.int "expecting int 1" "invalid int 1")
                    (Parser.token (Parser.Token "-a-" "expecting -a-"))
                    (Parser.int "expecting int 2" "invalid int 2")
                    (Parser.token (Parser.Token "-b-" "expecting -b-"))
                    (Parser.int "expecting int 3" "invalid int 3")
                    (\i1 () i2 () i3 -> Parser.succeed ( i1, i2, i3 ))
                )
                "123-a-456-b-x|end"
                |> Expect.equal
                    (Err
                        [ { row = 1
                          , col = 13
                          , problem = "expecting int 3"
                          , contextStack = []
                          }
                        ]
                    )
    , Test.test "bind5 error in 4 of 5" <|
        \() ->
            Parser.run
                (ParserExtra.bind5
                    (Parser.int "expecting int 1" "invalid int 1")
                    (Parser.token (Parser.Token "-a-" "expecting -a-"))
                    (Parser.int "expecting int 2" "invalid int 2")
                    (Parser.token (Parser.Token "-b-" "expecting -b-"))
                    (Parser.int "expecting int 3" "invalid int 3")
                    (\i1 () i2 () i3 -> Parser.succeed ( i1, i2, i3 ))
                )
                "123-a-456-x-789|end"
                |> Expect.equal
                    (Err
                        [ { row = 1
                          , col = 10
                          , problem = "expecting -b-"
                          , contextStack = []
                          }
                        ]
                    )
    , Test.test "bind5 error in 3 of 5" <|
        \() ->
            Parser.run
                (ParserExtra.bind5
                    (Parser.int "expecting int 1" "invalid int 1")
                    (Parser.token (Parser.Token "-a-" "expecting -a-"))
                    (Parser.int "expecting int 2" "invalid int 2")
                    (Parser.token (Parser.Token "-b-" "expecting -b-"))
                    (Parser.int "expecting int 3" "invalid int 3")
                    (\i1 () i2 () i3 -> Parser.succeed ( i1, i2, i3 ))
                )
                "123-a-x-b-789|end"
                |> Expect.equal
                    (Err
                        [ { row = 1
                          , col = 7
                          , problem = "expecting int 2"
                          , contextStack = []
                          }
                        ]
                    )
    , Test.test "bind5 error in 2 of 5" <|
        \() ->
            Parser.run
                (ParserExtra.bind5
                    (Parser.int "expecting int 1" "invalid int 1")
                    (Parser.token (Parser.Token "-a-" "expecting -a-"))
                    (Parser.int "expecting int 2" "invalid int 2")
                    (Parser.token (Parser.Token "-b-" "expecting -b-"))
                    (Parser.int "expecting int 3" "invalid int 3")
                    (\i1 () i2 () i3 -> Parser.succeed ( i1, i2, i3 ))
                )
                "123-x-456-b-789|end"
                |> Expect.equal
                    (Err
                        [ { row = 1
                          , col = 4
                          , problem = "expecting -a-"
                          , contextStack = []
                          }
                        ]
                    )
    , Test.test "bind5 error in 1 of 5" <|
        \() ->
            Parser.run
                (ParserExtra.bind5
                    (Parser.int "expecting int 1" "invalid int 1")
                    (Parser.token (Parser.Token "-a-" "expecting -a-"))
                    (Parser.int "expecting int 2" "invalid int 2")
                    (Parser.token (Parser.Token "-b-" "expecting -b-"))
                    (Parser.int "expecting int 3" "invalid int 3")
                    (\i1 () i2 () i3 -> Parser.succeed ( i1, i2, i3 ))
                )
                "x-a-456-b-789|end"
                |> Expect.equal
                    (Err
                        [ { row = 1
                          , col = 1
                          , problem = "expecting int 1"
                          , contextStack = []
                          }
                        ]
                    )
    ]


getParserResultTests : List Test
getParserResultTests =
    let
        testParser : Parser c String (ParserExtra.ParserResult c String Int)
        testParser =
            Parser.int "expecting int" "invalid int"
                |> ParserExtra.getParserResult

        sampleParser : Parser c String (ParserExtra.ParserResult c String Int)
        sampleParser =
            Parser.succeed identity
                |. Parser.token (Parser.Token "hello" "expecting hello")
                |= testParser
    in
    [ Test.test "testParser success" <|
        \() ->
            Parser.run sampleParser "hello123abc"
                |> Expect.equal (Ok (Ok 123))
    , Test.test "testParser failure" <|
        \() ->
            Parser.run sampleParser "helloabc123"
                |> Expect.equal
                    (Ok
                        (Err
                            [ { row = 1
                              , col = 1
                              , problem = "expecting int"
                              , contextStack = []
                              }
                            ]
                        )
                    )
    ]


getParserOutcomeTests : List Test
getParserOutcomeTests =
    let
        testParser : Parser c String ( String, ParserExtra.ParserResult c String ( Int, Int ) )
        testParser =
            Parser.int "expecting int" "invalid int"
                |> ParserExtra.getParserOutcome

        sampleParser : Parser c String ( String, ParserExtra.ParserResult c String ( Int, Int ) )
        sampleParser =
            Parser.succeed identity
                |. Parser.token (Parser.Token "hello" "expecting hello")
                |= testParser
    in
    [ Test.test "testParser success" <|
        \() ->
            Parser.run sampleParser "hello123abc"
                |> Expect.equal (Ok ( "123abc", Ok ( 123, 3 ) ))
    , Test.test "testParser failure" <|
        \() ->
            Parser.run sampleParser "helloabc123"
                |> Expect.equal
                    (Ok
                        ( "abc123"
                        , Err
                            [ { row = 1
                              , col = 1
                              , problem = "expecting int"
                              , contextStack = []
                              }
                            ]
                        )
                    )
    ]



-- HELPERS


successBacktrackableCheck : Parser c String a -> Parser c String String
successBacktrackableCheck parser =
    Parser.oneOf
        [ Parser.oneOf
            [ parser |> Parser.andThen (\_ -> Parser.problem "")
            , parser |> Parser.andThen (\_ -> Parser.succeed "backtrackable")
            ]
            |> Parser.backtrackable
        , Parser.succeed "committed"
        ]


failureBacktrackableCheck : Parser c String a -> Parser c String String
failureBacktrackableCheck parser =
    Parser.oneOf
        [ Parser.oneOf
            [ parser |> Parser.map (\_ -> "")
            , Parser.succeed "backtrackable"
            ]
            |> Parser.backtrackable
        , Parser.succeed "committed"
        ]


internalToolsTests : List Test
internalToolsTests =
    [ Test.test "backtrackable no error" <|
        \() ->
            Parser.run
                (Parser.succeed () |> successBacktrackableCheck)
                ""
                |> Expect.equal
                    (Ok "backtrackable")
    , Test.test "backtrackable error" <|
        \() ->
            Parser.run
                (Parser.problem "" |> failureBacktrackableCheck)
                ""
                |> Expect.equal
                    (Ok "backtrackable")
    , Test.test "committed no error" <|
        \() ->
            Parser.run
                (Parser.commit () |> Parser.andThen (\() -> Parser.succeed ()) |> successBacktrackableCheck)
                ""
                |> Expect.equal
                    (Ok "committed")
    , Test.test "committed error" <|
        \() ->
            Parser.run
                (Parser.commit () |> Parser.andThen (\() -> Parser.problem "") |> failureBacktrackableCheck)
                ""
                |> Expect.equal
                    (Ok "committed")
    ]
