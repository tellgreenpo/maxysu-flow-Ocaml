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
            bool * id Queue.t * path * id list


val bfs : int graph -> id list -> id -> id -> id Queue.t -> path option


val find_min : int graph -> path -> id ->int


val update_graph : int graph -> path -> int -> id ->int graph


val ford_fulkerson : int graph -> id -> id ->
                    id Queue.t -> int graph -> int -> (int graph * int)
