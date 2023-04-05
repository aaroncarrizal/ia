namespace Busqueda

module DFS =
    open Pila

    let estrategia = 
        {
            vaciar = empty
            insertar = push
            remover = pop
        }

    let key n = n.estado, n.profundidad