open Graph

let clone_nodes gr = n_fold gr new_node empty_graph

let gmap gr f = List.map (fun (id1, out) -> List.map (fun (id2, x) -> f id1 id2 x) out) gr


let add_arc g id1 id2 n =
  match find_arc g id1 id2 with
  | Some label -> 