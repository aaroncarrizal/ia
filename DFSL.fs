namespace Busqueda

module DFSL =
    open Pila

    let estrategia l = 
        {
            vaciar = empty
            insertar = 
                fun pila n -> 
                    if n.profundidad <= 1
                    then push pila n
                    else pila
            remover = pop
        }