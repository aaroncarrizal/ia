open Busqueda

module ExamenParcial2 =
    // Verdadero significa que ya cruzó, falso si no ha cruzado el río
    // pato * maiz * zorro * granjero
    type estado = bool * bool * bool * bool
    type acciones = CruzarPato | CruzarMaiz | CruzarZorro | RegresarSolo
    let inicio = (false, false, false, false)
    let sucesores (pato,maiz, zorro, granjero) =
        [
            (CruzarPato, (not pato, maiz, zorro, not granjero))
            (CruzarMaiz, (pato, not maiz, zorro, not granjero))
            (CruzarZorro, (pato, maiz, not zorro, not granjero))
            (RegresarSolo, (pato, maiz, zorro, not granjero))
        ]
        // Filtrar acciones inválidas
        // El pato no puede estar solo con el maiz sin el granjero, el pato no puede estar solo con el zorro sin el granjero
        |> List.filter (fun (_, (pato, maiz, zorro, granjero)) -> not (pato = maiz && pato <> granjero) && not (pato = zorro && pato <> granjero))

    let meta estado = estado = (true, true, true, true)
    let costo _ _ _ = 1.0
    let problema inicio =
        {
            inicio    = inicio
            meta      = meta
            sucesores = sucesores
            costo     = costo
        }

let solucion = Capitulo3.busquedaArbol (BFS.estrategia) (ExamenParcial2.problema (ExamenParcial2.inicio))
printf "%A" solucion

