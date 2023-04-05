namespace Busqueda

type cola<'a> = list<'a>
//@ es append
module Cola = 
    let empty = []
    let enqueue cola x = cola @ [x]
    let dequeue cola =
        match cola with
        | h :: t -> Some (h, t)
        | [] -> None