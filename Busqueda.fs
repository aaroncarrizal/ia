namespace Busqueda
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
        vaciar       : 'b
        insertar    : 'b -> nodo<'s, 'a> -> 'b
        remover     : 'b -> option<nodo<'s, 'a> * 'b>
    }

module Capitulo3 = 
    let nodoInicio estado = 
            {
                estado = estado
                profundidad = 0
                costo_ruta = 0.0
                accion = None
                padre = None
            }
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

    let busquedaArbol estrategia problema = 
        let raiz = nodoInicio problema.inicio
        let bolsa = estrategia.insertar 
                            estrategia.vaciar
                            raiz
        let rec loop bolsa = 
            match estrategia.remover bolsa with
            | Some (n, bolsa') ->
                if problema.meta n.estado
                then Some n
                else 
                    expandir problema n
                    |> List.fold estrategia.insertar bolsa'
                    |> loop
            | None -> None
        loop bolsa

module Capitulo4 = 
    open Capitulo3
    open System
    let asensionColina h problema = 
        let actual = nodoInicio problema.inicio
        let rec loop acual =
            let sucesores = expandir problema actual
            let vecino = List.minBy h sucesores
            if h vecino >= h actual
            then actual
            else loop vecino
        loop actual

    let temperatura T0 lambda t = 
        let x = T0 * Math.Exp(-lambda * t)
        if x < 1E-6
        then 0.0
        else x

    let recocidoSimulado seed h temperatura problema = 
        let rnd = Random(seed)
        let actual = nodoInicio problema.inicio
        let rec loop (t, actual) = 
            let T = temperatura t
            if T = 0.0
            then actual 
            else 
                let sucesores = expandir problema actual
                let i = rnd.Next(List.length sucesores)
                let next = List.item i sucesores
                let delta = h next - h actual
                if delta < 0.0 ||
                    rnd.NextDouble() <= Math.Exp (delta / T)
                then loop (t + 1.0, next)
                else loop (t + 1.0, actual)
        loop (1.0, actual)