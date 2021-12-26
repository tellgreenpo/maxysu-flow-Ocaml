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
    | (id2,x)::rest -> if (Bool.not (List.mem id2 visitedNodes)) && (x > 0)
        then (* The destination is not yet visited and the arc value is not 0 *)
            if id2=dst then (true,queue,((id2,id1)::foundPath) ,visitedNodes) (* Destination found*)
            else
                let ()=Queue.push id2 queue in (* Use a queue to push the visited node *)
                (* Loop again with updated visited nodes list and path*)
                arc_loop (id2::visitedNodes) id1 dst ((id2,id1)::foundPath) rest queue
        else arc_loop visitedNodes id1 dst foundPath rest queue


let print_path l=
    let rec hidden l=
    match l with
    | [] -> Printf.printf " . \n";
    | (x,y)::rest -> Printf.printf "(%i | %i) " x y; hidden rest
    in
    Printf.printf "Found path : ";
    hidden l



(* Path finding function *)

let bfs (graf : int graph) visitedNodes (source:id) (destination:id) queue=
    (* queue is for arc loop to update it *)
    let rec loop gr visited src dst q foundPath =
        let () = Printf.printf "Starting node : %i   " src in
        try
            let actualNode = Queue.take q in (* use arc_loop with the next element in the queue to explore *)
            let ()= Printf.printf "Actual exploring node : %i \n" actualNode in
            match arc_loop visited actualNode dst foundPath (out_arcs gr actualNode) q with
            (* Flag is true = path found => return Some path or continue searching *)
            | (flag,updatedQueue,udpatedPath,updatedVisitedNodes)->
            if flag=true then let () = print_path udpatedPath in Some udpatedPath else loop gr updatedVisitedNodes src dst updatedQueue udpatedPath (* call it with updated queue visit path*)
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
    let rec loop gr auxPath previousNode minimum source destination=
        let hidden g src dst =
        match (find_arc g src dst) with
        | None -> failwith "Arc in path does not exist in graph!"
        | Some x -> x
        in
    match auxPath with
    | [] -> Printf.printf "Found minimum flow : %i from %i -> %i\n" minimum source destination; minimum
    | (dst,src)::rest -> let smallestComparedCost = (Stdlib.min (hidden gr src dst) minimum) in
    (** Follow continuity of the path, nodes who do not lead to destination are ignored *)
    if dst=previousNode then  loop gr rest src smallestComparedCost src dst else loop gr rest previousNode minimum source destination
    in
    loop graf pathToEvaluate destination Int.max_int (-1) (-1)

let create_residual_graph gr =
    let add_reverse_arc g id1 id2 x = add_arc g id2 id1 0 in
    e_fold gr add_reverse_arc (gr)


let update_residual_graph (gr:int graph) pathToEvaluate maxFlow memoryNode=
    let rec hidden g inPath flow previousNode acuG=
    match inPath with
    | [] -> acuG
    | (dst,src)::rest -> if previousNode=dst then
            let g1 = add_arc acuG src dst (-flow) in
            let g2 = add_arc g1 dst src flow in
            hidden g rest flow src g2
        else
            hidden g rest flow previousNode acuG
    in
    hidden gr pathToEvaluate maxFlow memoryNode gr


let ford_fulkerson (gr : int graph) (src:id) (dst:id)=
    let rec hidden (g : int graph) (source:id) (destination:id) acuF=
        let  queue = Queue.create () in
        let () = Queue.push source queue in
        let res = bfs g [source] source destination queue in
        match res with
        | None -> (g,acuF)
        | Some foundPath -> Printf.printf "source destination : %i %i\n" source destination ;
                            let x=(find_min g foundPath destination) in
                            hidden (update_residual_graph g foundPath x destination) source destination (acuF+x)
    in
    hidden (create_residual_graph gr) src dst 0


let condition_reverse_arcs residualGraph g id1 id2 =
    match (find_arc residualGraph id2 id1) with
    | Some flow -> if flow>0 then Some flow else None
    | _ -> None

let fold_filter cond g id1 id2 lbl=
    match (cond g id1 id2) with
    | Some flow -> add_arc g id1 id2 flow
    | None -> g


let export_flow_graph (originalGraph:int graph) (residualGraph : int graph) =
    let flowGraph = (clone_nodes originalGraph) and cond = condition_reverse_arcs residualGraph in
    e_fold originalGraph (fold_filter cond) flowGraph
