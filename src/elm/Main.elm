module Main exposing (..)

import Html exposing (..)
import List
import Html.Attributes exposing (style, value)
import Html.Events exposing (onClick)
import Board
import Debug

import Dialog
import Dom exposing (focus)
import Settings
import Task

type Msg
    = Click Board.Cell Player
    | Reset
    | MsgSettings Settings.Msg
    | EditSettings
    | EditSettingsFocusAttempted


type Winner
    = Me
    | Opponent
    | Draw

type PlayerStatus
    = WaitingForOpponent
    | TakingTurn
    | GameOver Winner


type alias Player =
    { name : String
    , side : Board.Side
    , error : Maybe String
    , status : PlayerStatus
    }


playerX =
    { name = "X"
    , side = Board.X
    , error = Nothing
    , status = TakingTurn
    }


playerO =
    { name = "O"
    , side = Board.O
    , error = Nothing
    , status = WaitingForOpponent
    }

type alias Settings = {
    player1Name : String,
    player2Name : String,
    boardSize   : Int
}

type alias Model =
    { game    : Board.Game
    , player1 : Player
    , player2 : Player
    , settings : Settings
    , settingsForm : Maybe Settings.SettingsForm
    }



init : ( Model, Cmd Msg )
init =
    let
        settings = 
            { player1Name = "Player1"
            , player2Name = "Player2"
            , boardSize = 3
            }
    in
        ( initializeFromSettings settings, Cmd.none )

initializeFromSettings : Settings -> Model
initializeFromSettings settings =
    { game = Board.init settings.boardSize
    , player1 =
        { name = settings.player1Name
        , side = Board.X
        , error = Nothing
        , status = TakingTurn
        }
    , player2 =
        { name = settings.player2Name
        , side = Board.O
        , error = Nothing
        , status = WaitingForOpponent
        }
    , settings = settings
    , settingsForm = Nothing
    }

applySettings : Settings -> Model -> Model
applySettings settings model =
    let
        setName name player =
            { player | name = name }
        newPlayer1 = model.player1 |> setName settings.player1Name
        newPlayer2 = model.player2 |> setName settings.player2Name
    in
        { model |
            player1 = newPlayer1,
            player2 = newPlayer2
        }
        
view : Model -> Html Msg
view model =
    div []
        [ draw_field model.player1 model.game
        , draw_field model.player2 model.game
        , button [onClick (Reset) ][ text "Reset" ]
        , button [onClick EditSettings] [text "Edit Settings"]
        , Dialog.view (model.settingsForm |> dialogContent)
        ]

dialogContent maybeForm =
    case maybeForm of
        Nothing -> Nothing
        Just form -> Just (dialogConfig form)
    
dialogConfig form =
    { closeMessage = Nothing
    , containerClass = Nothing
    , header = Just (h3 [] [text "Settings"])
    , body = Just (Settings.view form |> Html.map MsgSettings)
    , footer = Just (Settings.footerView form |> Html.map MsgSettings)
    }

draw_field : Player -> Board.Game -> Html Msg
draw_field player game =
    div
        [ style
            [ ( "position", "relative" )
            , ( "height", "200px" )
            ]
        ]
        [
          div [] [ text player.name ]
        , div [] [ text (toString player.status) ]
        , div [] (List.map (draw_cell player) game.board)
        , div [] (draw_winning_series game)
        , div []
            [ case player.error of
                Just s ->
                    text s

                Nothing ->
                    text ""
            ]
        ]

draw_winning_series game =
    case game.status of
        Board.GameOver (Just winner) ->
            winner.series 
                |> List.concatMap draw_winning_serie
        Board.GameOver Nothing ->
            []
        Board.AwaitingForTurn _ ->
            []

draw_winning_serie serie =
    let
        symbol =
            case serie.direction of
                Board.Horizontal -> "-"
                Board.Vertical -> "|"
                Board.Diag1133 -> "\\"
                Board.Diag1331 -> "/"
        draw_cell cell = 
            div [
                style
                    [ ( "position", "absolute")
                    , ( "height", "45px" )
                    , ( "width", "45px" )
                    , ( "position", "absolute" )
                    , ( "line-height", "45px" )
                    , ( "text-align", "center" )
                    , ( "font-size", "45px" )
                    , ( "left", toString (50 * cell.x) ++ "px" )
                    , ( "top", toString (50 * cell.y) ++ "px" )
                    , ( "color", "black" )
                    , ( "z-index", "1" )
                    ]
            ] [ text symbol ] 
    in
        serie.cells |> List.map draw_cell


draw_cell : Player -> Board.Cell -> Html Msg
draw_cell player cell =
    let
        ( color, symbol ) =
            case cell.state of
                Nothing ->
                    ( "", "" )

                Just Board.X ->
                    ( "red", "X" )

                Just Board.O ->
                    ( "blue", "O" )
    in
        div
            [ onClick (Click cell player)
            , style
                [ ( "backgroundColor", "grey" )
                , ( "height", "45px" )
                , ( "width", "45px" )
                , ( "position", "absolute" )
                , ( "line-height", "45px" )
                , ( "text-align", "center" )
                , ( "font-size", "45px" )
                , ( "left", toString (50 * cell.x) ++ "px" )
                , ( "top", toString (50 * cell.y) ++ "px" )
                , ( "color", color )
                ]
            ]
            [ text symbol
            ]



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click cell player ->
            let
               (new_game, error) = (Board.try_apply_turn
                    { x = cell.x, y = cell.y, side = player.side }
                    model.game
                )


               new_model = { model | game = new_game }
                    |> clear_or_set_error_on_player player error
                    |> calculate_player_statuses
            in
                (new_model, Cmd.none)
        Reset ->
            ( initializeFromSettings model.settings, Cmd.none )
        EditSettingsFocusAttempted -> 
            ( model, Cmd.none )
        EditSettings ->
            case model.settingsForm of
                Just _ ->
                    (model, Cmd.none)
                Nothing ->
                    ( { model
                            | settingsForm = Just (Settings.loadForm model.settings)
                      }
                    , Task.attempt (\r -> EditSettingsFocusAttempted) (focus "settings-focus")
                    )
        MsgSettings payload ->
            let 
                justForm = case model.settingsForm of
                    Nothing -> Debug.crash "Oops"
                    Just form -> form
                (newModel, newCmd, newGlobalMsg) = Settings.update payload justForm
                mappedNewCmd =  Cmd.map MsgSettings newCmd
                handleGlobalEvent m =
                    case newGlobalMsg of
                        Nothing ->
                            m
                        Just (Settings.Submit newSettings) ->
                            { m
                                    | settings = newSettings
                                    , settingsForm = Nothing
                            } |> applySettings newSettings
                        Just (Settings.Cancel) ->
                            { m
                                  | settingsForm = Nothing
                            }
            in
                ( { model | settingsForm = Just newModel } |>  handleGlobalEvent, mappedNewCmd)


clear_or_set_error_on_player : Player -> Maybe String -> Model -> Model
clear_or_set_error_on_player player error model =
    case error of
        Just error_text ->
            model |> set_error_on_player player error_text
        Nothing ->
            model |> clear_error_on_player player

clear_error_on_player : Player -> Model -> Model
clear_error_on_player player model = 
    let
        newPlayer = { player | error = Nothing }
    in
        {
            model |
                player1 = if model.player1 == player then newPlayer else model.player1,
                player2 = if model.player2 == player then newPlayer else model.player2
        }

set_error_on_player : Player -> String -> Model -> Model
set_error_on_player player error model =
    let
        newPlayer = { player | error = Just error }
    in
        {
            model |
                player1 = if model.player1 == player then newPlayer else model.player1,
                player2 = if model.player2 == player then newPlayer else model.player2
        }

calculate_player_statuses : Model -> Model
calculate_player_statuses model =
    let
        get_status side = 
            case model.game.status of
                Board.AwaitingForTurn active_side ->
                    if active_side == side then TakingTurn else WaitingForOpponent
                Board.GameOver Nothing ->
                    GameOver Draw
                Board.GameOver (Just winner) ->
                    if winner.side == side then
                        GameOver Me
                    else
                        GameOver Opponent
        new_player1 = model.player1 |> \p -> { p | status = get_status(model.player1.side) }
        new_player2 = model.player2 |> \p -> { p | status = get_status(model.player2.side) }
    in
        {
            model |
                player1 = new_player1,
                player2 = new_player2
        }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
