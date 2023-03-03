FROM ocaml/opam2:ubuntu-20.04-ocaml-4.11

USER opam
RUN opam update && opam switch create 4.11.1
RUN opam install dune
RUN opam install js_of_ocaml tyxml
