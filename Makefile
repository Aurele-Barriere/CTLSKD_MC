all: common.ml logics.ml models.ml ltl_mc.ml ctls_mc.ml ctlskd_mc.ml main.ml nusmv_wrapper.c
	gcc nusmv_wrapper.c -o nusmv_wrapper
	ocamlc common.ml logics.ml models.ml ltl_mc.ml ctls_mc.ml ctlskd_mc.ml main.ml -o mc.out

clean:
	-rm *.cmi
	-rm *.cmo
	-rm *.out
	-rm ltl
	-rm output
	-rm nusmv_wrapper
