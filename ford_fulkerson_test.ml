open Graph
open Tools
open Gfile
open Ford_fulkerson

let nodes = [1;2;3;4]

let arcs = [(1,2,2);(1,3,4);
            (2,3,3);(2,4,1);
            (3,4,5)]


let test_graph = create_graph_from_txt nodes arcs


let test_arc_loop =
    let q = Queue.create ()in
    match (arc_loop [] 1 6 [] (out_arcs test_graph 1) q) with
    | (found,queue,foundPath,visitedNodes) ->
    Printf.printf "========= Test de arc loop =============================== \n";
    Printf.printf "Found : %B\n" found;
    print_queue queue;
    print_path foundPath;
    print_visited_nodes visitedNodes

let test_bfs=
    let q =Queue.create () in  let ()=Queue.push 1 q in
    Printf.printf "========= Test de bfs =============================== \n";
    match (bfs test_graph [] 1 6 q) with
    | None -> Printf.printf "No path found\n";
    | Some foundPath ->
    print_path foundPath

let test_find_min =
    Printf.printf "========= Test de find min =============================== \n";
     let q =Queue.create () in  let ()=Queue.push 1 q in
     match (bfs test_graph [] 1 6 q) with
    | None -> Printf.printf "No path found\n";
    | Some foundPath ->
    Printf.printf "    Found min : %i\n" (find_min test_graph foundPath 6)

let test_create_residual =
    Printf.printf "========= Test de create residual graph=============================== \n";
    let g = create_residual_graph test_graph in
    write_file "result_resi.txt" (gmap g string_of_int); export "result_resi.txt" "result_resi"



let test_ford_fulkerson =
    Printf.printf "========= Test de ford fulkerson=============================== \n";
    match (ford_fulkerson test_graph 1 4 ) with
    | (g,f) -> Printf.printf "Found flow : %i\n" f ; write_file "result.txt" (gmap g string_of_int); export "result.txt" "result"


let test_export_flow_graph =
    match (ford_fulkerson test_graph 1 4 ) with
    | (g,_) ->  let flowGraph = export_flow_graph test_graph g in
    write_file "flow.txt" (gmap flowGraph string_of_int); export "flow.txt" "flow"
