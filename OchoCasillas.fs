namespace Busqueda

module OchoCasillas = 
    type accion = 
            | Left
            | Right
            | Up 
            | Down
        

    type estado = list<int>
    
    let inicio = [7; 2; 4; 5; 0; 6; 8; 3; 1]

    let estado_meta = [1; 2; 3; 4; 5; 6; 7]

    let costo _ _ _ = 1.0

    let meta estado = 
        List.map2 (fun x y -> (x, y))
            estado_meta estado
        |> List.forall (fun (x, y) -> x = y)

    let cero estado = 
        List.findIndex (fun x -> x = 0) estado

    let suc indice accion (estado: estado) = 
        let swap i j = 
            estado 
            |> List.mapi(fun index x -> 
                            if index = i
                            then List.item j estado
                            elif index = j
                            then List.item i estado
                            else x)
        match accion with
        | Left -> accion, swap indice (indice-1)
        | Right -> accion, swap indice (indice + 1)
        | Up -> accion, swap indice (indice - 3)
        | Down -> accion, swap indice (indice + 3)

    let succesores estado =
        let indice = cero estado
        [
            if indice % 3 <> 0
            then suc indice Left estado
            if indice % 3 <> 2
            then suc indice Right estado
            if indice > 2 
            then suc indice Up estado
            if indice < 6
            then suc indice Down estado
        ]
    
    let problema estado = 
        {
            inicio = estado
            sucesores = succesores
            meta = meta
            costo = costo
        }
    (*type estado2 int[] []s
    type estado3 list<int>
    type estado4 int * int * int * int * int*)

    let h1 nodo = 
        List.zip nodo.estado estado_meta
        |> List.sumBy (fun (x, y) ->
                if x <> 0 && x <> y
                then 1.0
                else 0.0)
