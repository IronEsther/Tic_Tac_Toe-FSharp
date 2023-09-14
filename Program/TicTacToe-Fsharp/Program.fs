
type Player = 
    | None
    | X
    | O

type Board = Player array array

let emptyBoard = 
    [|[| None; None; None |];
      [| None; None; None |];
      [| None; None; None |]|]

let printBoard (board: Board) =
    for row in board do
        for cell in row do
            match cell with
            | None -> printf ". "
            | X -> printf "X "
            | O -> printf "O "
        printfn ""

let hasWon (player: Player) (board: Board) =
    let allSame (line: Player array) = Array.forall (fun cell -> cell = player) line
    let anyRow = board |> Array.exists allSame
    let anyColumn = [0..2] |> List.exists (fun i -> board |> Array.map (fun row -> row.[i]) |> allSame)
    let diagonal1 = [| board.[0].[0]; board.[1].[1]; board.[2].[2] |]
    let diagonal2 = [| board.[0].[2]; board.[1].[1]; board.[2].[0] |]
    anyRow || anyColumn || allSame diagonal1 || allSame diagonal2

let togglePlayer player =
    match player with
    | X -> O
    | O -> X
    | None -> None

let rec playGame board currentPlayer =
    printBoard board
    printfn "%A's Turn!" currentPlayer
    printf "Wähle eine Reihe (0-2): "
    let row = int (System.Console.ReadLine())
    printf "Wähle eine Zeile (0-2): "
    let col = int (System.Console.ReadLine())
    
    if row >= 0 && row <= 2 && col >= 0 && col <= 2 && board.[row].[col] = None then
        board.[row].[col] <- currentPlayer
        if hasWon currentPlayer board then
            printfn "%A Gewinnt!" currentPlayer
        else if Array.forall (fun row -> Array.forall (fun cell -> cell <> None) row) board then
            printfn "Unentschieden!"
        else
            playGame board (togglePlayer currentPlayer)
    else
        printfn "Du kleiner Rabauke, das ist nicht erlaubt"
        playGame board currentPlayer

[<EntryPoint>]
let main argv =
    playGame emptyBoard X
    0
