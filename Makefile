all: common.ml logics.ml models.ml ltl_mc.ml ctls_mc.ml ctlskd_mc.ml main.ml
	ocamlc common.ml logics.ml models.ml ltl_mc.ml ctls_mc.ml ctlskd_mc.ml main.ml -o mc.out

clean:
	-rm *.cmi
	-rm *.cmo
	-rm *.out
