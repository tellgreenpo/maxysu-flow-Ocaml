open Graph

let clone_nodes gr = n_fold gr new_node empty_graph

let gmap gr f = e_fold gr (fun g id1 id2 x -> new_arc g id1 id2 (f x)) clone_nodes


let add_arc g id1 id2 n =
  match find_arc g id1 id2 with
  | Some label -> new_arc g id1 id2 (label+n)
  | None -> new_arc g id1 id2 n