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
let rec arc_loop visitedNodes (id1: id) (dst:id) (foundPath:path) arcs queue =
    match arcs with
    | [] -> (false,queue,foundPath,visitedNodes) (* Destination still not found so we signal it with the boolean *)
    | (id2,x)::rest -> if (Bool.not (List.mem id2 visitedNodes)) && (x >= 0)
        then (* The destination is not yet visited and the arc value is not 0 *)
            if id2=dst then (true,queue,((id2,id1)::foundPath) ,visitedNodes) (* Destination found*)
            else
                let ()=Queue.push id2 queue in (* Use a queue to push the visited node *)
                (* Loop again with updated visited nodes list and path*)
                arc_loop (id2::visitedNodes) id1 dst ((id2,id1)::foundPath) rest queue
        else arc_loop visitedNodes id1 dst foundPath rest queue



(*Use list for visited add id *)

(* Pour tester
let resultTest1 =
    let compare a b = a>b and zero = 0 and queue = Queue.create () in
    arc_loop [1] 1 5 [] [(3,2);(7,8);(9,5);(156,98)] queue compare zero

let restulQueueSize element =
    match element with
    | q,_,_-> Queue.length q
*)




(* Path finding function *)

let bfs (graf : int graph) visitedNodes (source:id) (destination:id) queue=
    (* queue is for arc loop to update it *)
    let rec loop gr visited src dst q foundPath =
        try
            let actualNode = Queue.take q in (* use arc_loop with the next element in the queue to explore *)
            match arc_loop visited actualNode dst foundPath (out_arcs gr actualNode) q with
            (* Flag is true = path found => return Some path or continue searching *)
            |(flag,updatedQueue,udpatedPath,updatedVisitedNodes)->
            if flag=true then Some udpatedPath else loop gr updatedVisitedNodes src dst updatedQueue udpatedPath (* call it with updated queue visit path*)
        with
        | Queue.Empty -> None (* Path not found => we return None *)
    in
    loop graf visitedNodes source destination queue []

(** The rest of the code works only for integer graph *)

(**residual graph with edges and reverse edges *)
(*let add_reverse_edges_graph gr = e_fold gr (fun g id1 id2 x -> new_arc g id2 id1 0) (clone_nodes gr) *)
(** Use add arc directly to make add negative value if needed *)


(**Max posible flow for a path *)
let find_min graf (pathToEvaluate:path) (destination:id)=
    let rec loop gr auxPath minimum previousNode =
        let hidden g src dst =
        match (find_arc g src dst) with
        | None -> failwith "Arc in path does not exist in graph!"
        | Some x -> x
        in
    match auxPath with
    | [] -> Printf.printf "Found minimum flow : %i\n" minimum; minimum
    | (dst,src)::rest -> let smallestComparedCost = (Stdlib.min (hidden gr src dst) minimum) in
    (** Follow continuity of the path nodes who do not lead to destination are ignored *)
    if dst=previousNode then  loop gr rest smallestComparedCost src else loop gr rest minimum previousNode
    in
    loop graf pathToEvaluate Int.max_int destination


(** Conception problem => new graph everytime??? Seems good but need to test it*)
(** Update arc from path or add it if it doesn't exist(reverse edge) and if it is not from the path then it simply copies it from the original precedent*)
let rec update_graph (gr:int graph) (pathToEvaluate:path) maxflow previousNode =
    match pathToEvaluate with
    | [] -> gr
    | (dst,src)::rest -> if dst=previousNode then
        update_graph (e_fold gr
        (fun g id1 id2 x-> if (id1=src)&&(id2=dst) then new_arc g id1 id2 (-maxflow) else if (id2=src)&&(id1=dst) then add_arc g id1 id2 (maxflow) else new_arc g id1 id2 x)
        (clone_nodes gr)) rest maxflow src
    else update_graph gr rest maxflow previousNode


(* Tant qu'on trouve encore un chemin possible => continuer a modifier le graph et maxFlow *)
(* Termine par retourner le graph d'ecart et le flot maximum *)

(** !!!! Initialize queue with starting point !!!! *)
let ford_fulkerson (gr : int graph) (src:id) (dst:id) queue acuGraph acuFlow=
    let rec hidden (g : int graph) (source:id) (destination:id) q acuG acuF=
        let res = bfs g [] source destination q in
        match res with
        | None -> (acuG,acuF)
        | Some chemin -> let x=(find_min gr chemin destination) in
                hidden g source destination q (update_graph g chemin x destination) (acuF+x)
    in
    let () = Queue.push src queue in
    hidden gr src dst queue acuGraph acuFlow
