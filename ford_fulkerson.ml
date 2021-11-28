open Graph
open Tools
open Queue
open Array
open Bool


(*Ne pas utiliser les exceptions => bit monnot*)

(* Check destinations from a node *)

let rec arc_loop visited (id1: id) (dst:id) path arcs queue (compare: 'a -> 'a -> bool) (zero: 'a)=
    match arcs with
    | [] -> (false,queue,path,visited) (* Destination still not found so we signal it with the boolean *)
    | (id2,x)::rest -> if (Bool.not (List.mem id2 visited)) && (compare x zero)
        then
            if id2=dst then (true,queue,((id2,id1)::path),visited) (* Destination found *)
            else
                let ()=Queue.push id2 queue in
                arc_loop (id2::visited) id1 dst ((id2,id1)::path) rest queue compare zero
        else arc_loop visited id1 dst path rest queue compare zero



(*Use list for visited add id *)

(* Pour tester
let resultTest1 = 
    let compare a b = a>b and zero = 0 and queue = Queue.create () in
    arc_loop [1] 1 5 [] [(3,2);(7,8);(9,5);(156,98)] queue compare zero

let restulQueueSize element =
    match element with
    | q,_,_-> Queue.length q
*)




(* Main loop while path found *)

let bfs (gr : 'a graph) visited (src:id) (dst:id) queue path (compare: 'a -> 'a -> bool) (zero: 'a)=
    let rec loop gr visited src dst queue path compare zero=
        try
            let u = Queue.take queue in
            match arc_loop visited u dst path (out_arcs gr u) queue compare zero with
            |(flag,queue,path,visited)-> if flag then Some path else loop gr visited src dst queue path compare zero
        with
        | Queue.Empty -> None
    in
    loop gr visited src dst queue path compare zero

(** The rest of the code works only for integer graph *)

(**residual graph with edges and reverse edges *)
(*let add_reverse_edges_graph gr = e_fold gr (fun g id1 id2 x -> new_arc g id2 id1 0) (clone_nodes gr) *)
(** Use add arc directly to make add negative value if needed *)


(**Max flow possible for a path *)
let find_min path =
    let rec loop path min = 
    match path with
    | [] -> min
    | (dst,src)::rest -> let mini = Int.min (find_arc src dst) min in loop rest mini
    in
    loop path Int.max_int

(** Need to add on reverse edge and substract on edge*)

(** Conception problem => new graph everytime??? I think its good but need to teste it*)
(** Update arc from path or add it if it doesn't exist(reverse edge) and if it is not from the path then it simply copies it from the original precedent*)
let rec update_graph gr path maxflow=
    match path with
    | [] -> gr
    | (dst,src)::rest -> update_graph (e_fold gr 
        (fun g id1 id2 x-> if (id1=src)&&(id2=dst) then add_arc g id1 id2 (-maxflow) else if (id2=src)&&(id1=dst) then add_arc g id1 id2 (maxflow) else add_arc g id1 id2 x)
        (clone_nodes gr)) rest maxflow



let rec ford_fulkerson (gr : 'a graph) (src:id) (dst:id) (compare: 'a -> 'a -> bool) (zero: 'a) queue acu=
    let maxFlow = 0 and
    path = bfs gr [] src dst queue [] compare zero in
    match path with
    | None -> acu
    | Some path -> ford_fulkerson gr src dst compare zero queue (update_graph gr path (find_min ath))