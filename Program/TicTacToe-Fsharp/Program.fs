
type Player = 
    | None
    | X
    | O

type Board = Player array array

let emptyBoard = 
    [|[| None; None; None |];
      [| None; None; None |];
      [| None; None; None |]|]

// Zeigt das aktuelle Spielfeld an
let printBoard (board: Board) =
    printfn "Aktuelles Spielfeld:\n"
    for row in board do
        for cell in row do
            match cell with
            | None -> printf ". "
            | X -> printf "X "
            | O -> printf "O "
        printfn ""

// Überprüft, ob der Spieler gewonnen hat
let hasWon (player: Player) (board: Board) =
    let allSame (line: Player array) = Array.forall (fun cell -> cell = player) line
    let anyRow = board |> Array.exists allSame
    let anyColumn = [0..2] |> List.exists (fun i -> board |> Array.map (fun row -> row.[i]) |> allSame)
    let diagonal1 = [| board.[0].[0]; board.[1].[1]; board.[2].[2] |]
    let diagonal2 = [| board.[0].[2]; board.[1].[1]; board.[2].[0] |]
    anyRow || anyColumn || allSame diagonal1 || allSame diagonal2

// Wechselt den Spieler
let togglePlayer player =
    match player with
    | X -> O
    | O -> X
    | None -> None

// Überprüft, ob ein Zug gültig ist
let isValidMove (board:Board) row col =
    row >= 0 && row <= 2 && col >= 0 && col <= 2 && board.[row].[col] = None

// Eingabeaufforderung für den Spieler
let rec getInput prompt =
    printf "%s" prompt
    match System.Int32.TryParse(System.Console.ReadLine()) with
    | (true, value) when value >= 0 && value <= 2 -> value
    | _ -> 
        printfn "Ungültige Eingabe! Bitte gib eine Zahl zwischen 0 und 2 ein."
        getInput prompt

// Hauptspiellogik
let rec playGame board currentPlayer =
    printBoard board
    printfn "%A ist dran!" currentPlayer
    let row = getInput "Wähle Zeile (0-2): "
    let col = getInput "Wähle Spalte (0-2): "

    if isValidMove board row col then
        board.[row].[col] <- currentPlayer
        if hasWon currentPlayer board then
            printfn "\nHerzlichen Glückwunsch! %A hat gewonnen!" currentPlayer
        else if Array.forall (fun row -> Array.forall (fun cell -> cell <> None) row) board then
            printfn "\nDas Spiel endet unentschieden!"
        else
            playGame board (togglePlayer currentPlayer)
    else
        printfn "\nDu kleiner Rabauke, das ist nicht erlaubt."
        playGame board currentPlayer

[<EntryPoint>]
let main argv =
    printfn "Willkommen bei Tic Tac Toe!\n"
    playGame emptyBoard X
    0

