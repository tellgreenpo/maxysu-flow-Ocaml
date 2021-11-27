open Graph
open Tools
open Queue
open Array
open Bool


(*Ne pas utiliser les exceptions => bit monnot*)


let rec arc_loop visited (id1: id) path arcs queue (compare: 'a -> 'a -> bool) (zero: 'a)=
    match arcs with
    | [] -> (queue,path,visited)
    | (id2,x)::rest -> if Bool.not (List.mem id1 visited)  then
        if id1=dst then (queue,((id2,id1)::path),visited) else
        arc_loop (id2::visited) id1 ((id2,id1)::path) rest (Queue.push id2 queue)



(*Use list for visited add id *)



(* Main loop while path found *)
(*
let Bfs (gr: 'a graph) (visited: id List) (src:id) (dst:id) queue (path:(id*id) List) (compare: 'a -> 'a -> bool) (zero: 'a)=
    let rec loop gr visited src dst queue path compare zero=
        try
            let u = Queue.take queue in
        with
        | Queue.Empty -> path
        | _ -> try arc_loop visited u path (out_arcs u) queue compare zero
            with
            | Reached_Destination p -> p
            | (queue,path,visited)-> loop gr visited src dst queue path compare zero
    in
    loop gr visited src dst queue path compare zero
*)