namespace Busqueda
open System

module nreinas =
    type estado = int list 
    type accion = unit
    let inicio n =
        let rnd = Random()
        [1..n]
        |> List.map(fun _ -> rnd.Next(n))

    let sucesores estado = 
        let mover columna = 
            List.mapi( fun i _ ->
                List.mapi( fun j x ->
                    if j <> columna
                    then x
                    else i) estado) estado
            |> List.filter( fun estado' -> estado' <> estado)
        estado
        |> List.mapi(fun columna _ -> mover columna)
        |> List.collect id
        |> List.map( fun estado -> ((), estado))

    let atacan ((x1: int, y1: int), (x2: int, y2: int)) =
        if y1 = y2 || Math.Abs(x2 - x1) = Math.Abs (y2 - y1) then
            1 
        else 0

    //funcion heuristica
    let paresAtacando estado = 
        let reinas = 
            estado
            |> List.mapi(fun col fila -> (col, fila))
        reinas
        |> List.allPairs reinas
        |> List.filter( fun (r1, r2) -> r1 <> r2)
        |> List.sumBy atacan
        |> (fun n -> n/2)

    let meta estado = 
        paresAtacando estado = 0 

    let problema n = 
        {
            inicio = inicio n
            sucesores = sucesores
            meta = meta
            costo = fun _ _ _ -> 1.0
        }