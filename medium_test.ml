open Tools
open Graph
open Gfile
open Ford_fulkerson

let cars = [1;2;3;4]
let stations = [5;6;7;8]
let reachableStations = [[5;6;8];
                        [5;6;7];
                        [5;7];
                        [5;6;7]]

let resultGraph = create_car_station_graph cars stations reachableStations

let test_export =
    write_file "cars.txt" (gmap resultGraph string_of_int);
    export "cars.txt" "cars"


let solution =
    match (ford_fulkerson resultGraph 0 9 ) with
        | (g,f) ->  Printf.printf "Found max: %i\n" f ; let flowGraph = export_flow_graph resultGraph g in
        write_file "flow.txt" (gmap flowGraph string_of_int); export "flow.txt" "flow"
