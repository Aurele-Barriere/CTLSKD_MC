all: common.ml logics.ml models.ml ltl_mc.ml ctls_mc.ml ctlskd_mc.ml
	ocamlc common.ml logics.ml models.ml ltl_mc.ml ctls_mc.ml ctlskd_mc.ml

clean:
	-rm *.cmi
	-rm *.cmo
	-rm *.out
