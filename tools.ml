open Graph


(*Error: This expression has type (id * 'b out_arcs) list
but an expression was expected of type 'a graph -> 'b graph *)
let clone_nodes (gr: 'a graph): 'a graph -> 'b graph =
  let rec hidden gr acu= 
    match gr with
    |[]->acu
    |(id,_)::rest-> let arcs: 'b out_arcs = [] in hidden rest ((id,arcs)::acu)
  in
  hidden gr []

let gmap gr f = assert false
let add_arc g id1 id2 n = assert false