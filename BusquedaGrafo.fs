namespace Busqueda1
//'s = estado, 'a = accion, 'b = bolsa
type problema<'s, 'a> = 
    {
        inicio      : 's
        sucesores   : 's -> list<'a * 's>
        meta        : 's -> bool
        costo       : 's -> 'a -> 's -> float
    }
    
//cosas opcionales
(*type option <'a> = 
    | None
    | Some of 'a *)

type nodo<'s, 'a> = 
    {
        profundidad : int
        costo_ruta  : float
        estado      : 's
        accion      : option<'a>
        padre       : option<nodo<'s, 'a>>
    }

type estrategia<'s, 'a, 'b> = 
    {
        vacia       : 'b
        insertar    : 'b -> nodo<'s, 'a> -> 'b
        remover     : 'b -> option<nodo<'s, 'a> * 'b>
    }

module Capitulo3 = 
    (*let rec map f xs  =
        match xs with
        | [] -> []
        | h :: t -> f h :: map f t*)
    let append xs ys = List.fold (fun xs x -> x :: xs) ys xs

    let expandir problema padre = 
        problema.sucesores padre.estado
        |> List.map (fun (a, s) -> 
            {
                profundidad = padre.profundidad + 1
                accion = Some a
                padre = Some padre
                estado = s
                costo_ruta = padre.costo_ruta + problema.costo padre.estado a s  //costo del padre mas 
            }
            )

    let busquedaGrafo key estrategia problema = 
        let raiz = 
            {
                estado = problema.inicio
                profundidad = 0
                costo_ruta = 0
                accion = None
                padre = None
            }
        let bolsa = estrategia.insertar 
                            estrategia.vacia
                            raiz
        let rec loop (bolsa, procesados) = 
            match estrategia.remover bolsa with
            | Some (n, bolsa') ->
                if problema.meta n.estado
                then Some n
                else 
                    if Set.contains (key n) procesados
                    then loop (bolsa', procesados)
                    else
                        expandir problema n
                        |> List.fold estrategia.insertar bolsa'
                        |> (fun bolsa -> loop (bolsa, Set.add (key n) procesados))
            | None -> None
        loop (bolsa, Set.empty)