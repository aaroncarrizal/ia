namespace Busqueda

module Greedy = 
    let estrategia h = 
        {
            vaciar = Map.empty
            insertar = 
                fun pqueue n ->
                    Map.add (h n, n.estado)
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

    let key h n = n.estado, h n