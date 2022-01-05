open Graph

(* returns a new graph having the same nodes than gr, but no arc. *)
val clone_nodes : 'a graph -> 'b graph

(* maps all arcs of gr by function f. *)
val gmap : 'a graph -> ('a -> 'b) -> 'b graph

(* adds n to the value of the arc between id1 and id2. If the arc does not exist, it is created. *)
val add_arc: int graph -> id -> id -> int -> int graph

val create_graph_from_txt : id list -> (id * id * int) list -> int graph

val print_queue : id Queue.t -> unit

val print_visited_nodes : id list -> unit

val print_path : (id * id) list -> unit

val export_flow_graph : int graph -> int graph -> int graph

val create_car_station_graph : int list -> int list -> (int list) list -> int graph
