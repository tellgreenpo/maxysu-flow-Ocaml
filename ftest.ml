open Gfile
open Tools
open Stdlib
open Ford_fulkerson

let () =
  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf "\nUsage: %s infile source sink outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)

  (* These command-line arguments are not used for the moment. *)
  and _source = int_of_string Sys.argv.(2)
  and _sink = int_of_string Sys.argv.(3)
  in

  (*(* Open file *)
  let graph = from_file infile in

  (* Rewrite the graph that has been read. *)
  let () = write_file outfile graph in*)
  let gr=from_file  infile in
  let gint= gmap gr int_of_string in
  match (ford_fulkerson gint _source _sink) with
  | (g,f) -> Printf.printf "Found max flow : %i\n" f;
  let flowGraph = export_flow_graph gint g in
    write_file "outfile.txt" (gmap flowGraph string_of_int); export "outfile.txt" outfile
