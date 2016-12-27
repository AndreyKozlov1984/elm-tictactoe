module Board exposing (..)
import List exposing(..)
type Side
  = X
  | O

type alias Cell = {
    x : Int,
    y : Int,
    state : Maybe Side
}

type alias Board = 
    List Cell

type Direction
    = Horizontal 
    | Vertical
    | Diag1133
    | Diag1331

type alias WinningSerie = {
    cells : List Cell,
    direction : Direction
}

type alias Winner = {
    side : Side,
    series : List WinningSerie
}

type GameStatus
    = AwaitingForTurn Side
    | GameOver (Maybe Winner)
    
type alias Game = {
    board : Board,
    status : GameStatus
}

type alias Turn = {
    x : Int,
    y : Int,
    side : Side
}


try_apply_turn : Turn -> Game -> (Game, Maybe String) 
try_apply_turn turn game =
    let
        cell_or_null = game.board |> filter (\c -> c.x == turn.x && c.y == turn.y) |> head
        x = 1
    in
        case cell_or_null of
            Nothing ->
                (game, Just "Wrong Cell")
            Just cell ->
                if cell_busy cell then
                    (game, Just "Cell is busy")
                else if wrong_turn turn game then
                    (game, Just "It is not your turn")
                else
                    (game |> apply_turn turn, Nothing)

apply_turn : Turn -> Game -> Game
apply_turn turn game =
    game
        |> update_board turn
        |> check_end_game
        |> change_turn

init : Int -> Game
init size =
    let 
        xList = range 1 size
        yList = range 1 size
        board = yList |> concatMap (\y -> xList |> map (\x -> { x = x, y = y, state = Nothing }) )
    in
        {
            status = AwaitingForTurn X,
            board = board
        }

-- Specific methods

cell_busy : Cell -> Bool
cell_busy cell = 
    cell.state /= Nothing

wrong_turn : Turn -> Game -> Bool
wrong_turn turn game =
    game.status /= AwaitingForTurn turn.side

update_board : Turn -> Game -> Game
update_board turn game =
    let 
        convert : Cell -> Cell
        convert c = 
            if c.x == turn.x && c.y == turn.y then { c | state = Just turn.side } else c
        new_board = game.board |> map convert
    in
        { game | board = new_board }

check_end_game : Game -> Game
check_end_game game =
    { game | status = game_over_status game }

game_over_status: Game -> GameStatus
game_over_status game =
    let
        winning_series_x = winning_series X game.board
        winning_series_o = winning_series O game.board
    in
        if not (isEmpty winning_series_x) then
            GameOver (Just {side = X, series = winning_series_x})
        else if not (isEmpty winning_series_o) then
            GameOver (Just {side = O, series = winning_series_o})
        else if out_of_space game.board then
            GameOver Nothing
        else
            game.status


winning_series : Side -> Board -> List WinningSerie
winning_series side board =
    let
        size = board |> length |> toFloat |> sqrt |> truncate

        horizontal_line : Int -> WinningSerie
        horizontal_line i =
            board
                |> filter (\c -> c.y == i)
                |> (\cells -> { cells = cells, direction = Horizontal })

        vertical_line : Int -> WinningSerie
        vertical_line i =
            board
                |> filter (\c -> c.x == i)
                |> (\cells -> { cells = cells, direction = Vertical })

        diagonal_line_1 : WinningSerie
        diagonal_line_1 =
            {
                cells = filter (\c -> c.x == c.y) board,
                direction = Diag1133
            }

        diagonal_line_2 : WinningSerie
        diagonal_line_2 =
            {
                cells = filter (\c -> c.x == size + 1 - c.y) board,
                direction = Diag1331
            }

        good_field : Cell -> Bool
        good_field cell = 
            cell.state == Just side

        is_winning_serie : WinningSerie -> Bool
        is_winning_serie serie =
            serie.cells
                |> all good_field

        all_lines : List WinningSerie
        all_lines = concat [
            (range 1 size) |> map horizontal_line,
            (range 1 size) |> map vertical_line,
            [ diagonal_line_1 ],
            [ diagonal_line_2 ]
        ]
    in
        all_lines 
            |> filter is_winning_serie 

    

out_of_space : Board -> Bool
out_of_space board =
    all (\c -> c.state /= Nothing) board

change_turn : Game -> Game
change_turn game =
    let new_status = case game.status of
        AwaitingForTurn side ->
            AwaitingForTurn (if side == X then O else X)
        GameOver _ ->
            game.status
    in
        { game | status = new_status }
