# maxysu-flow-Ocaml

Ocaml project:
http://wwwperso.insa-toulouse.fr/~lebotlan/Y/ocaml.html

## To compile and run the ford fulkerson algorithm on a sample graph:
ocamlbuild ftest.ml ftest.native
./ftest.native infile_graph outfile_no_file_type source sink
## To visualize the obtained graph
dot -Tsvg outfile_no_file_type > outfile_no_file_type.svg

## To compile and the medium project
ocamlbuild medium_test.ml medium_test.native
./medium_test.native
## To visualize the obtained graph
dot -Tsvg cars > cars.svg
