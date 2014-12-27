clean :
	rm *.cm*

top :
	ocamlfind ocamlc -c  -g  -w -40-26  -package "compiler-libs.common,batteries" parse_ocaml.ml
	ocamlfind ocamlc -c  -g   -package "compiler-libs.common,batteries" parse_ocaml.cmo object_lingua.ml
	utop -init ocaml2lang.ml


cmt:
	for i in tst*.ml ; do ocamlfind ocamlc -package batteries -bin-annot $$i ; done
	rm tst*.cmo
	rm tst*.cmi
