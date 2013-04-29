pazcal: pazcal.ml
		ocamlc pazcal.ml -o pazcal

pazcal.ml: pazcal.mll
		ocamllex pazcal.mll

clean:
	${RM} pazcal.ml *.cmi *.cmo

distclean: clean
	${RM} pazcal
