

prog: main.byte
	js_of_ocaml --toplevel main.byte

main.byte: 
	ocamlfind ocamlc -package js_of_ocaml -package js_of_ocaml-ppx -package autobill -linkpkg -o main.byte main.ml

clean:
	rm -rf main.byte main.cmo main.cmi main.js 