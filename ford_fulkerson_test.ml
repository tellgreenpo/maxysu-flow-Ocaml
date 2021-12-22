open Graph
open Tools
open Gfile
open Ford_fulkerson

let nodes = [1;2;3;4;5;6]

let arcs = [(1,2,5);
            (1,3,21);
            (2,4,9);(2,6,70);
            (3,4,3);
            (4,5,24);
            (5,6,11)]

(** This works *)
let rec create_graph_nodes g nodesToAdd =
    match nodesToAdd with
    | [] -> g
    | src::rest-> create_graph_nodes (new_node g src) rest

let rec create_graph_arcs g arcsToAdd =
    match arcsToAdd with
    | [] -> g
    | (src,dst,cost)::rest -> create_graph_arcs (add_arc g src dst cost) rest

let test_graph = create_graph_arcs (create_graph_nodes empty_graph nodes) arcs

let print_queue q =
    let rec hidden q =
    match (Queue.take_opt q) with
    | None -> Printf.printf "\n"
    | Some x -> Printf.printf "%i " x ; hidden q
    in
    Printf.printf "Queue values : ";
    hidden q

let print_visited_nodes l =
    let rec hidden l=
    match l with
    | [] -> Printf.printf "\n"
    | x::rest -> Printf.printf "%i " x; hidden rest
    in
    Printf.printf "Visited Nodes : ";
    hidden l

let print_path l=
    let rec hidden l=
    match l with
    | [] -> Printf.printf "\n"
    | (x,y)::rest -> Printf.printf "(%i | %i) " x y; hidden rest
    in
    Printf.printf "Found path : ";
    hidden l



let test_arc_loop =
    let q = Queue.create ()in
    match (arc_loop [] 1 6 [] (out_arcs test_graph 1) q) with
    | (found,queue,foundPath,visitedNodes) ->
    Printf.printf "========= Test de arc loop =============================== \n";
    Printf.printf "Expected value : False  Found : %B\n" found;
    Printf.printf "Expected queue values : 3 2    ";
    print_queue queue;
    Printf.printf "Expected path : (2 | 1) (3 | 1)    ";
    print_path foundPath;
    Printf.printf "Expected visited nodes :  2 3    ";
    print_visited_nodes visitedNodes

let test_bfs=
    let q =Queue.create () in  let ()=Queue.push 1 q in
    Printf.printf "========= Test de bfs =============================== \n";
    match (bfs test_graph [] 1 6 q) with
    | None -> Printf.printf "No path found\n";
    | Some foundPath -> print_path foundPath

let none = 5
