namespace Busqueda

module CostoUniforme= 
    let estrategia a = 
        {
            vaciar = Map.empty
            insertar = 
                fun pqueue n ->
                    Map.add (n.costo_ruta, n.estado)
                        n pqueue
            remover = 
                fun pqueue ->
                    match Map.tryFindKey 
                        (fun _ _ -> true) 
                        pqueue with
                    | Some k -> Some (Map.find k pqueue,
                                        Map.remove k pqueue)
                    | None -> None
        }

    let key n = n.estado, n.costo_ruta