module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String

import Board

field : String -> Board.Board
field s =
    s
        |> String.lines
        |> List.filter (\l -> (l |> String.words) /= [""])
        |> List.indexedMap
            (\line_index line ->
                line
                    |> String.words
                    |> List.indexedMap
                        (\word_index word ->
                            {
                                x = word_index + 1,
                                y = line_index + 1,
                                state = 
                                    if word == "X" then
                                        Just Board.X
                                    else if word == "O" then
                                        Just Board.O
                                    else
                                        Nothing
                            }
                        )
            ) |> List.concat

initial_board : Board.Game
initial_board = Board.init 3

occupied_board : Board.Game
occupied_board =
    {
        board = field """
                      X X X
                      O O *
                      * * *
                      """,
        status = Board.GameOver (Just {
                    side = Board.X,
                    series = [
                      {
                          direction = Board.Horizontal,
                          cells = [
                                { x = 1, y = 1, state = Just Board.X},
                                { x = 2, y = 1, state = Just Board.X},
                                { x = 3, y = 1, state = Just Board.X}
                          ]
                      }
                    ]
                })
    }

all : Test
all =
    describe "Board" [
        describe "test" [
            test "winning series are fine" <|
                \() ->
                    Expect.equal
                        (Board.winning_series Board.X (field """
                                                               X X
                                                               O *
                                                           """)
                        )
                        [
                            {
                                direction = Board.Horizontal,
                                cells = [
                                    { x = 1, y = 1, state = Just Board.X},
                                    { x = 2, y = 1, state = Just Board.X}
                                ]
                            }
                        ]
            ,test "winning series are fine" <|
                \() ->
                    Expect.equal
                        (Board.winning_series Board.X (field """
                                                             * X *
                                                             * X *
                                                             * X *
                                                           """)
                        )
                        [
                            {
                                direction = Board.Vertical,
                                cells = [
                                    { x = 2, y = 1, state = Just Board.X},
                                    { x = 2, y = 2, state = Just Board.X},
                                    { x = 2, y = 3, state = Just Board.X}
                                ]
                            }
                        ]
            ,test "winning series are fine" <|
                \() ->
                    Expect.equal
                        (Board.winning_series Board.X (field """
                                                             * * X
                                                             * X *
                                                             X X *
                                                           """)
                        )
                        [
                            {
                                direction = Board.Diag1331,
                                cells = [
                                    { x = 3, y = 1, state = Just Board.X},
                                    { x = 2, y = 2, state = Just Board.X},
                                    { x = 1, y = 3, state = Just Board.X}
                                ]
                            }
                        ]
            ,test "winning series are fine" <|
                \() ->
                    Expect.equal
                        (Board.winning_series Board.X (field """
                                                             * * X
                                                             * X *
                                                             O X *
                                                           """)
                        )
                        []
           ,test "field helper is ok" <|
                \() ->
                    Expect.equal [
                        { x = 1, y = 1, state = Just Board.X},
                        { x = 2, y = 1, state = Just Board.X},
                        { x = 1, y = 2, state = Just Board.O},
                        { x = 2, y = 2, state = Nothing}
                    ] (field """
                            X X
                            O *
                            """)
        ],
        describe "Board.init"
                [ test "it works properly" <|
                    \() ->
                        Expect.equal (Board.init 2) {
                            board = field """
                                * *
                                * *
                            """
                            , status = Board.AwaitingForTurn Board.X
                        }
                ]
        ,describe "Board.try_apply_turn" [
                   test "start of game" <|
                     \() ->
                         Expect.equal
                            (
                             initial_board
                                |> Board.try_apply_turn { x = 1, y = 2, side = Board.X }
                            )
                            ({
                                board = field """
                                  * * *
                                  X * *
                                  * * *
                                """
                                ,status = Board.AwaitingForTurn Board.O
                            }, Nothing)

                   ,test "wrong turn - cell is occupied" <|
                     \() ->
                         let game = {
                                        board = field """
                                            * * *
                                            X * *
                                            * * *
                                        """
                                        , status = Board.AwaitingForTurn Board.O
                                    }
                         in
                             Expect.equal
                                (Board.try_apply_turn {x = 1, y = 2, side = Board.O} game)
                                (game, Just "Cell is busy")
                   ,test "wrong turn - it is not your turn!" <|
                     \() ->
                         let game = initial_board
                         in
                             Expect.equal
                                (Board.try_apply_turn {x = 1, y = 2, side = Board.O} game)
                                (game, Just "It is not your turn")

                   ,test "wrong turn - game over!" <|
                     \() ->
                         let
                            game = occupied_board
                         in
                            Expect.equal
                                (Board.try_apply_turn {x = 3, y = 2, side = Board.O} game)
                                (game, Just "It is not your turn")
                  ,test "detect game over!" <|
                      \() ->
                         let
                            game =
                                {
                                    board = field """
                                         X X *
                                         O O *
                                         * * *
                                    """
                                    ,status = Board.AwaitingForTurn Board.X
                                }
                            turn =
                                { x = 3, y = 1, side = Board.X}
                         in
                             Expect.equal
                                (Board.try_apply_turn turn game)
                                ({
                                     board = field """
                                         X X X
                                         O O *
                                         * * *
                                     """
                                     ,status = Board.GameOver (Just {
                                        side = Board.X,
                                        series = [
                                            {
                                                direction = Board.Horizontal,
                                                cells = [
                                                    { x = 1, y = 1, state = Just Board.X},
                                                    { x = 2, y = 1, state = Just Board.X},
                                                    { x = 3, y = 1, state = Just Board.X}
                                                ]
                                            }
                                        ]
                                             
                                    })
                                }, Nothing)
                ]
        ]
