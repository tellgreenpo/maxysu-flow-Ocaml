open Graph
open Tools
open Queue
open Array
open Bool
open Int
open Stdlib

type path = (id*id) list
(* Check destinations from a node *)
(* Compare function needs to be implemented for different type of label *)
let rec arc_loop visited (id1: id) (dst:id) (pat:path) arcs queue (compare: int -> int -> bool) (zero: int)=
    match arcs with
    | [] -> (false,queue,pat,visited) (* Destination still not found so we signal it with the boolean *)
    | (id2,x)::rest -> if (Bool.not (List.mem id2 visited)) && (compare x zero)
        then
            if id2=dst then (true,queue,((id2,id1)::pat),visited) (* Destination found *)
            else
                let ()=Queue.push id2 queue in (* Use a queue to push the nodes *)
                arc_loop (id2::visited) id1 dst ((id2,id1)::pat) rest queue compare zero
        else arc_loop visited id1 dst pat rest queue compare zero



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

let bfs (gr : int graph) visited (src:id) (dst:id) queue (chemin:path) (compare: int -> int -> bool) (zero: int)=
    (* queue is for arc loop to update it *)
    let rec loop gr visited src dst queue chemin compare zero=
        try
            let u = Queue.take queue in (* use arc_loop with the next element in the queue to explore *)
            match arc_loop visited u dst chemin (out_arcs gr u) queue compare zero with
            (* Flag is true = path found => return Some path or continue searching *)
            |(flag,q,p,visit)-> if flag then Some chemin else loop gr visit src dst q p compare zero (* call it with updated queue visit path*)
        with
        | Queue.Empty -> None (* Path not found => we return None *)
    in
    loop gr visited src dst queue chemin compare zero

(** The rest of the code works only for integer graph *)

(**residual graph with edges and reverse edges *)
(*let add_reverse_edges_graph gr = e_fold gr (fun g id1 id2 x -> new_arc g id2 id1 0) (clone_nodes gr) *)
(** Use add arc directly to make add negative value if needed *)


(**Max posible flow for a path *)
let find_min gr (chemin:path) =
    let rec loop gr chemin mi = 
    let hidden gr src dst = 
    match (find_arc gr src dst) with
    | None -> failwith "Arc in path does not exist in graph!"
    | Some x -> x
    in
    match chemin with
    | [] -> mi
    | (dst,src)::rest -> let mini = (Stdlib.min (hidden gr src dst) mi) in loop gr rest mini
    in
    loop gr chemin Int.max_int


(** Conception problem => new graph everytime??? I think its good but need to teste it*)
(** Update arc from path or add it if it doesn't exist(reverse edge) and if it is not from the path then it simply copies it from the original precedent*)
let rec update_graph (gr:int graph) (chemin:path) maxflow=
    match chemin with
    | [] -> gr
    | (dst,src)::rest -> update_graph (e_fold gr 
        (fun g id1 id2 x-> if (id1=src)&&(id2=dst) then add_arc g id1 id2 (-maxflow) else if (id2=src)&&(id1=dst) then add_arc g id1 id2 (maxflow) else add_arc g id1 id2 x)
        (clone_nodes gr)) rest maxflow


(* Tant qu'on trouve encore un chemin possible => continuer a modifier le graph et maxFlow *)
(* Termine par retourner le graph d'ecart et le flot maximum *)
let rec ford_fulkerson (gr : int graph) (src:id) (dst:id) (compare: int -> int -> bool) (zero: int) queue acuGraph acuFlow=
    let 
    res = bfs gr [] src dst queue [] compare zero in
    match res with
    | None -> (acuGraph,acuFlow)
    | Some chemin -> let x=(find_min gr chemin) in
                ford_fulkerson gr src dst compare zero queue (update_graph gr chemin x) (acuFlow+x)