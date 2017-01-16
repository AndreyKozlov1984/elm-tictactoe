-- get it working when we implicitly fix a validation
module Settings exposing (..)
import Html exposing (Html)
import Html exposing (node, form, div, button, span, h1, label, input, text)
import Html.Attributes exposing (id, style, value, class, classList)
import Html.Events exposing (onFocus, onBlur, onInput, onClick)

type alias Settings =
    { player1Name : String
    , player2Name : String
    , boardSize : Int
    }

type Field
    = Player1Name
    | Player2Name
    | BoardSize

type alias SettingsForm =
    { player1Name : String
    , player1NameErrors : List String
    , player1NameHadFocus: Bool
    , player2Name : String
    , player2NameErrors : List String
    , player2NameHadFocus: Bool
    , boardSize : String
    , boardSizeErrors : List String
    , boardSizeHadFocus: Bool
    , hasErrors : Bool
    }

loadForm : Settings -> SettingsForm
loadForm values =
    {
        player1Name = values.player1Name,
        player1NameErrors = [],
        player1NameHadFocus = True,
        player2Name = values.player2Name,
        player2NameErrors = [],
        player2NameHadFocus = True,
        boardSize = toString values.boardSize,
        boardSizeErrors = [],
        boardSizeHadFocus = True,
        hasErrors = False
    }

type Msg
    = FocusIn Field
    | FocusOut Field
    | Change Field String
    | TrySubmit
    | TryCancel

type OutMsg
    = Cancel
    | Submit Settings

update : Msg -> SettingsForm -> (SettingsForm, Cmd Msg, Maybe OutMsg)
update cmd model =
    case cmd of
        Change Player1Name value ->
            ( { model | player1Name = value } 
            , Cmd.none
            , Nothing
            )
        Change Player2Name value ->
            ( { model | player2Name = value } 
              , Cmd.none
              , Nothing
            )
        Change BoardSize value ->
            ( { model | boardSize = value } 
            , Cmd.none
            , Nothing
            )
        FocusIn _ ->
            ( model
            , Cmd.none
            , Nothing
            )

        FocusOut Player1Name ->
            ( { model
                | player1NameHadFocus = True
              } |> liveValidate
            , Cmd.none
            , Nothing
            )

        FocusOut Player2Name ->
            ( { model
                | player2NameHadFocus = True
              } |> liveValidate
            , Cmd.none
            , Nothing
            )
        FocusOut BoardSize ->
            ( { model 
                  | boardSizeHadFocus = True
              } |> liveValidate
            , Cmd.none
            , Nothing
            )

        TryCancel ->
            ( model
            , Cmd.none
            , Just Cancel
            )
        TrySubmit ->
            let 
                newModel = model |> validate
            in
                ( newModel
                , Cmd.none
                , if newModel.hasErrors then
                      Nothing
                  else
                      Just (Submit ( modelFromForm newModel ))
                )

    
validatePlayer1NameMinimumLength : SettingsForm -> Maybe String
validatePlayer1NameMinimumLength model =
    let
        value = model.player1Name
        length = String.length value
    in
        if length < 2 then Just "Player 1 name should be at least 2 symbols" else Nothing

validatePlayer1NameMaximumLength : SettingsForm -> Maybe String
validatePlayer1NameMaximumLength model =
    let
        value = model.player1Name
        length = String.length value
    in
       if length > 10 then Just "Player 1 name should be maximum 10 symbols" else Nothing

validatePlayerNamesAreDifferent : SettingsForm -> Maybe String
validatePlayerNamesAreDifferent model =
    let
        player1 = model.player1Name
        player2 = model.player2Name
    in
        if (String.length player1) > 0 && (String.length player2) > 0 then
            if player1 == player2 then
                Just "Player1 and Player2 should have different names"
            else
                Nothing
        else
            Nothing

validatePlayer1Name : SettingsForm -> List String
validatePlayer1Name model =
    [ validatePlayer1NameMinimumLength model
    , validatePlayer1NameMaximumLength model
    , validatePlayerNamesAreDifferent model
    ] |> List.filterMap identity

validatePlayer1NameIfRequired : SettingsForm -> List String
validatePlayer1NameIfRequired model =
    if model.player1NameHadFocus then
        validatePlayer1Name model
    else
        []

validatePlayer2NameMinimumLength : SettingsForm -> Maybe String
validatePlayer2NameMinimumLength model =
    let
        value = model.player2Name
        length = String.length value
    in
        if length < 2 then Just "Player 1 name should be at least 2 symbols" else Nothing

validatePlayer2NameMaximumLength : SettingsForm -> Maybe String
validatePlayer2NameMaximumLength model =
    let
        value = model.player2Name
        length = String.length value
    in
       if length > 10 then Just "Player 2 name should be maximum 10 symbols" else Nothing

validatePlayer2Name : SettingsForm -> List String
validatePlayer2Name model =
    [ validatePlayer2NameMinimumLength model
    , validatePlayer2NameMaximumLength model
    , validatePlayerNamesAreDifferent model
    ] |> List.filterMap identity

validatePlayer2NameIfRequired : SettingsForm -> List String
validatePlayer2NameIfRequired model =
    if model.player2NameHadFocus then
        validatePlayer2Name model
    else
        []
   
    
validateBoardSizeNumeric : SettingsForm -> Maybe String
validateBoardSizeNumeric model =
    let
        value = model.boardSize
        result = String.toInt value
    in
        case result of
            Ok _ ->
                Nothing
            Err _ ->
                Just "Board size should be a numeric value"

validateBoardSizeMinimumValue : SettingsForm -> Maybe String
validateBoardSizeMinimumValue model =
    let
        result = String.toInt model.boardSize
    in
        case result of
            Ok number ->
                if number < 2 then
                    Just "Board size should be at least 2"
                else
                    Nothing
            Err _ ->
                Nothing

validateBoardSizeMaximumValue : SettingsForm -> Maybe String
validateBoardSizeMaximumValue model =
    let
        result = String.toInt model.boardSize
    in
        case result of
            Ok number ->
                if number > 5 then
                    Just "Board size should be no more than 5"
                else
                    Nothing
            Err _ ->
                Nothing

validateBoardSize : SettingsForm -> List String
validateBoardSize model =
    [ validateBoardSizeNumeric model
    , validateBoardSizeMinimumValue model
    , validateBoardSizeMaximumValue model
    ] |> List.filterMap identity

validateBoardSizeIfRequired : SettingsForm -> List String
validateBoardSizeIfRequired model =
    if model.boardSizeHadFocus then
        validateBoardSize model
    else
        []

liveValidate model =
    { model
        | player1NameErrors = validatePlayer1NameIfRequired model
        , player2NameErrors = validatePlayer2NameIfRequired model
        , boardSizeErrors = validateBoardSizeIfRequired model
        , hasErrors = [
            validatePlayer1Name model,
            validatePlayer2Name model,
            validateBoardSize model
        ] |> List.concat |> List.isEmpty |> not
    }

validate model =
    {  model 
           | player1NameErrors = validatePlayer1Name model
           , player2NameErrors = validatePlayer2Name model
           , boardSizeErrors = validateBoardSize model
           , hasErrors = [
                             validatePlayer1Name model,
                             validatePlayer2Name model,
                             validateBoardSize model
                         ] |> List.concat |> List.isEmpty |> not
    }

modelFromForm : SettingsForm -> Settings
modelFromForm form =
    { player1Name = form.player1Name
    , player2Name = form.player2Name
    , boardSize = String.toInt form.boardSize |> Result.withDefault 0
    }

drawError : String -> Html Msg
drawError error =
    div [] [text (error)]

view : SettingsForm -> Html Msg
view model =
    div
        [ style []
        , class "form-horizontal"
        ]
        [ field
            { label = "Player 1 Name"
            , id = Player1Name
            , value = model.player1Name
            , errors = model.player1NameErrors
            }
        , field
            { label = "Player 2 Name"
            , id = Player2Name
            , value = model.player2Name
            , errors = model.player2NameErrors
            }
        , field
            { label = "Board Size"
            , id = BoardSize
            , value = model.boardSize
            , errors = model.boardSizeErrors
            }
        ]

footerView model =
    div [class "well"]
        [ button [class "btn btn-default", onClick TryCancel] [text "Cancel"]
        , button [class "btn btn-success", onClick TrySubmit] [text "Submit"]
        ]

field options =
    div [ classList
            [ ("form-group", True)
            , ("has-error", options.errors |> List.isEmpty |> not)
            ]
        ]
        [ label [class "col-sm-4 control-label"] [text options.label]
        , div 
            [ class "col-sm-8" ] 
            [ input
                [ id "settings-focus"
                , class "form-control"
                , onFocus (FocusIn options.id)
                , onBlur (FocusOut options.id)
                , onInput (Change options.id)
                , value options.value
                ]
                []
            , div
                [ class "help-block"
                , style 
                    [ ("min-height", "20px")
                    , ("margin-bottom", "0px")
                    , ("margin-top", "0px")
                    ] 
                ] 
                (options.errors |> List.map drawError)
            ]
        ]
    
