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
    