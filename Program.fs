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
    printfn "Enter multiple lines of text (press Enter on an empty line to finish):"
    let lines = readMultipleLines()
    printf "%A" lines

main()

