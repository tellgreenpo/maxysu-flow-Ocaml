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



(** This does not *)
let test_arc_loop =
    let q = Queue.create () in
    let tuple = arc_loop [] 5 6 [] (out_arcs test_graph 5) q in
