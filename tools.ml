open Graph

let clone_nodes gr = n_fold gr new_node empty_graph

let gmap gr f = e_fold gr (fun g id1 id2 x -> new_arc g id1 id2 (f x)) (clone_nodes gr)


let add_arc g id1 id2 n =
  match find_arc g id1 id2 with
  | Some label -> new_arc g id1 id2 (label+n)
  | None -> new_arc g id1 id2 n

let rec create_graph_nodes g nodesToAdd =
  match nodesToAdd with
  | [] -> g
  | src::rest-> create_graph_nodes (new_node g src) rest

let rec create_graph_arcs g arcsToAdd =
    match arcsToAdd with
    | [] -> g
    | (src,dst,cost)::rest -> create_graph_arcs (add_arc g src dst cost) rest

let create_graph_from_txt nodes arcs =
  create_graph_arcs (create_graph_nodes empty_graph nodes) arcs

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
    | [] -> Printf.printf " . \n";
    | (x,y)::rest -> Printf.printf "(%i | %i) " x y; hidden rest
    in
    Printf.printf "Found path : ";
    hidden l

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
