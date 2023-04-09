open Busqueda
open System

let parseStringToIntList (input: string) : int list =
    input.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
    |> Array.toList
    |> List.map Int32.Parse

let readMultipleLines() =
    let rec readLines acc =
        let line = Console.ReadLine()
        if String.IsNullOrWhiteSpace(line) then
            List.rev acc
        else
            let line2 = parseStringToIntList line
            readLines (line2::acc)
    readLines []

let main() =
    // printfn "Enter multiple lines of text (press Enter on an empty line to finish):"
    // let lines = readMultipleLines()
    // printf "%A" lines

        // Función para verificar si un número es válido en una casilla del sudoku
    let rec isValid (board:int[][]) row col num =
        let mutable valid = true

        // Verificar si el número se repite en la misma fila
        for i in 0 .. 8 do
            if board.[row].[i] = num then
                valid <- false

        // Verificar si el número se repite en la misma columna
        for i in 0 .. 8 do
            if board.[i].[col] = num then
                valid <- false

        // Verificar si el número se repite en el mismo cuadro de 3x3
        let startRow = (row / 3) * 3
        let startCol = (col / 3) * 3
        for i in startRow .. startRow + 2 do
            for j in startCol .. startCol + 2 do
                if board.[i].[j] = num then
                    valid <- false

        // Si no se encontraron repeticiones, el número es válido
        valid

    // Función para resolver el sudoku utilizando backtracking
    // Función para resolver el sudoku utilizando backtracking
    let rec solveSudoku (board:int[][]) row col =
        if row = 9 then
            true // Si se llega al final del tablero, está resuelto
        else if col = 9 then
            solveSudoku board (row + 1) 0 // Si se llega al final de una fila, pasar a la siguiente fila
        else if board.[row].[col] <> 0 then
            solveSudoku board row (col + 1) // Si la casilla ya tiene un número, pasar a la siguiente casilla
        else
            // Casilla vacía encontrada, probar números del 1 al 9
            for num in 1 .. 9 do
                if isValid board row col num then
                    // Si el número es válido, se coloca en la casilla
                    board.[row].[col] <- num

                    // Se llama recursivamente a la función para seguir probando
                    if solveSudoku board row (col + 1) then
                        true // Si se resuelve el sudoku, retornar verdadero
                    else
                        // Si la recursión no lleva a una solución, se retrocede y se borra el número
                        board.[row].[col] <- 0
                        false

            false // Si no se encontró ninguna solución, retornar falso


    // Ejemplo de uso
    let sudokuBoard =
        [|
            [| 2; 5; 8; 7; 3; 6; 9; 4; 1 |]
            [| 6; 1; 9; 8; 2; 4; 3; 5; 7 |]
            [| 4; 3; 7; 9; 1; 5; 2; 6; 8 |]
            [| 3; 9; 5; 2; 7; 1; 4; 8; 6 |]
            [| 7; 6; 2; 4; 9; 8; 1; 3; 5 |]
            [| 8; 4; 1; 6; 5; 3; 7; 2; 9 |]
            [| 1; 8; 4; 3; 6; 9; 5; 7; 2 |]
            [| 5; 7; 6; 1; 4; 2; 8; 9; 3 |]
            [| 9; 2; 3; 5; 8; 7; 6; 1; 0 |]
        |]

    // Función para imprimir el tablero del sudoku en la consola
    let printBoard (board:int[][]) =
        for row in board do
            printfn "%A" row

    printfn "Sudoku inicial:"
    printBoard sudokuBoard

    // Resolver el sudoku
    if solveSudoku sudokuBoard 0 0 then
        printfn "Sudoku resuelto:"
        printBoard sudokuBoard
    else
        printfn "No se pudo resolver el sudoku."

main()

