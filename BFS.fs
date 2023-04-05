namespace Busqueda

module BFS = 
    open Cola
    let estrategia = 
        {
            vaciar = empty
            insertar = enqueue
            remover = dequeue
        }
    
    let key n = n.estado

