module Main exposing (..)

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
    { form : Settings.SettingsForm
    , settings : Settings.Settings
    , isDialogOpen : Bool
    }

settings = 
    { player1Name = "p1"
    , player2Name = ""
    , boardSize = 13
    }

init : ( Model, Cmd Msg )
init =
    ({ form = Settings.loadForm settings
     , settings = settings
     , isDialogOpen = False
     } , Cmd.none)


view : Model -> Html Msg
view model =
    div [class "container"]
        [ div [class "jumbotron"]
              [ h1 [] [text "Current Values:"]
              , div [] [text (toString model.settings)]
              ]
        , button [onClick Edit] [text "Edit Settings"]
        , div [] (if model.isDialogOpen then
                     [ h1 [] [text "Embedded Form!"]
                       , Settings.view model.form |> Html.map MsgSettings
                     ]
                  else
                     []
                 )
        ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FocusAttempted -> 
            (model, Cmd.none)
        Edit ->
            case model.isDialogOpen of
                True ->
                    (model, Cmd.none)
                False ->
                    ( { model
                            | isDialogOpen = True 
                            , form = Settings.loadForm model.settings
                      }
                    , Task.attempt (\r -> FocusAttempted) (focus "settings-focus")
                    )
        MsgSettings payload ->
            let 
                (newModel, newCmd, newGlobalMsg) = Settings.update payload model.form
                mappedNewCmd =  Cmd.map MsgSettings newCmd
                handleGlobalEvent m =
                    case newGlobalMsg of
                        Nothing ->
                            m
                        Just (Settings.Submit newSettings) ->
                            { m
                                    | settings = newSettings
                                    , isDialogOpen = False
                            }
                        Just (Settings.Cancel) ->
                            { m
                                  | isDialogOpen = False
                            }
            in
                ( { model | form = newModel } |>  handleGlobalEvent, mappedNewCmd)
                
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
