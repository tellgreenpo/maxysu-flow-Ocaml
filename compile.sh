ocamlc -c graph.mli graph.ml gfile.mli gfile.ml tools.mli tools.ml
ocamlc -o ftest graph.cmo gfile.cmo tools.cmo ftest.ml
