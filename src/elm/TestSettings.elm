module Main exposing (..)

import Debug
import Dialog
import Html exposing (..)
import List
import Html.Attributes exposing (class, style, value)
import Html.Events exposing (onClick)
import Dom exposing (focus)
import Task

import Settings

type Msg
    = MsgSettings Settings.Msg
    | Edit
    | FocusAttempted

type alias Model =
    { form : Maybe Settings.SettingsForm
    , settings : Settings.Settings
    }

settings = 
    { player1Name = "p1"
    , player2Name = ""
    , boardSize = 13
    }

init : ( Model, Cmd Msg )
init =
    ({ form = Nothing
     , settings = settings
     } , Cmd.none)


view : Model -> Html Msg
view model =
    div [class "container"]
        [ div [class "jumbotron"]
              [ h1 [] [text "Current Values:"]
              , div [] [text (toString model.settings)]
              ]
        , button [onClick Edit] [text "Edit Settings"]
        , Dialog.view (model.form |> dialogContent)
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



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FocusAttempted -> 
            (model, Cmd.none)
        Edit ->
            case model.form of
                Just _ ->
                    (model, Cmd.none)
                Nothing ->
                    ( { model
                            | form = Just (Settings.loadForm model.settings)
                      }
                    , Task.attempt (\r -> FocusAttempted) (focus "settings-focus")
                    )
        MsgSettings payload ->
            let 
                justForm = case model.form of
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
                                    , form = Nothing
                            }
                        Just (Settings.Cancel) ->
                            { m
                                  | form = Nothing
                            }
            in
                ( { model | form = Just newModel } |>  handleGlobalEvent, mappedNewCmd)
                
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
