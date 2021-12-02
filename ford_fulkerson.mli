open Graph
open Tools
open Queue
open Array
open Bool
open Int
open Stdlib

type path = (id*id) list

val arc_loop : id list -> id -> id -> path -> int out_arcs ->
            id Queue.t ->
            (int -> int -> bool) -> int -> bool * id Queue.t * path * id list


val bfs : int graph -> id list -> id -> id -> id Queue.t -> path ->
            (int -> int -> bool) -> int -> path option


val find_min : int graph -> path -> int


val update_graph : int graph -> path -> int -> int graph


val ford_fulkerson : int graph -> id -> id -> (int -> int -> bool) -> int -> 
                    id Queue.t -> int graph -> int -> (int graph * int)
