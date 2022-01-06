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

(** To check because i was vibing with music*)
(** Returns graph with cars and source *)
let add_cars (carList : int list) =
    let rec hidden acu lst=
    match lst with
    | [] -> acu
    | x::rest -> hidden (new_node acu x) rest
    in
    hidden (new_node empty_graph 0) carList

(** Connexion de chaque noeud voiture avec la source 0 **)
let connect_car_source (g : int graph) carList=
    let rec hidden acu l=
    match l with
    | [] -> acu
    | x::rest -> hidden (new_arc acu 0 x 1) rest
    in
    hidden g carList

(** Ajoute dans un graphe toutes les stations de la liste **)
let add_stations (g : int graph) (stationList : int list) =
    let rec hidden acu l index=
    match l with
    | [] -> new_node acu (index+1)
    | x::rest -> hidden (new_node acu x) rest x
    in
    hidden g stationList 0

(** Connecter toutes les stations a la source des stations **)
let connect_station_sink (g:int graph) (stationList : int list)=
    let rec hidden acu l dst =
    match l with
    | [] -> acu
    | x::rest -> hidden (new_arc acu x dst 1) rest dst
    and sink = (List.hd (List.rev stationList)+1) in
    hidden g stationList sink

(** On connecte la voiture aux stations qu'elle peut attendre **)
(** Tous les arcs valent 1 **)
let link_cars_stations (g :int graph) (car: int) (stations: int list)=
    let rec hidden (acu : int graph) (index: int) (l: int list) =
    match l with
    | [] -> acu
    | station::rest -> hidden (new_arc acu index station 1) index rest
    in
    hidden g car stations


(** Cars start at 1 *)
(** carList : liste des voitures **)
(** stationList : liste des stations **)
(** reachable : liste de listes des station atteignables pour chaque voitures **)
let create_car_station_graph carList stationList reachable=
    let rec hidden (g:int graph) reachableStations car=
    match reachableStations with
    | [] -> g
    | stations::rest -> hidden (link_cars_stations g car stations) rest (car+1)
    and g1 = add_cars carList in
    let g2 = connect_car_source g1 carList in
    let g3 = add_stations g2 stationList in
    let g4 = connect_station_sink g3 stationList in
    hidden g4 reachable 1
