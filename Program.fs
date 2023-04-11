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

let convertListToArray (input: int list list) : int[][] =
    input
    |> List.map (fun innerList -> innerList |> Array.ofList)
    |> Array.ofList


let main() =
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
    let rec solveSudoku (board:int[][]) row col =
        if row = 9 then
            true // Si se llega al final del tablero, está resuelto
        else if col = 9 then
            solveSudoku board (row + 1) 0 // Si se llega al final de una fila, pasar a la siguiente fila
        else if board.[row].[col] <> 0 then
            solveSudoku board row (col + 1) // Si la casilla ya tiene un número, pasar a la siguiente casilla
        else
            // Casilla vacía encontrada, probar números del 1 al 9
            let mutable result = false // Se utiliza mutable para almacenar el resultado de la recursión
            for num in 1 .. 9 do
                if isValid board row col num then
                    // Si el número es válido, se coloca en la casilla
                    board.[row].[col] <- num

                    // Se llama recursivamente a la función para seguir probando
                    if solveSudoku board row (col + 1) then
                        // Si se resuelve el sudoku, se actualiza el resultado y se sale del bucle
                        result <- true
                    else
                        // Si la recursión no lleva a una solución, se retrocede y se borra el número
                        board.[row].[col] <- 0

            result // Se retorna el resultado de la recursión al final de la función


    printfn "Dame el sudoku inicial:"
    let lines = readMultipleLines()
    let sudokuBoard = convertListToArray lines

    // Función para imprimir el tablero del sudoku en la consola
    let printBoard (board:int[][]) =
        for row in board do
            for col in row do
                printf "%i " col
            printfn ""

    // Resolver el sudoku
    if solveSudoku sudokuBoard 0 0 then
        printfn "Sudoku resuelto:"
        printBoard sudokuBoard
    else
        printfn "No se pudo resolver el sudoku."

main()

