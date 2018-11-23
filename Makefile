all: common.ml logics.ml models.ml print.ml ltl_mc.ml ctls_mc.ml ctlskd_mc.ml main.ml nusmv_wrapper
	ocamlc common.ml logics.ml models.ml print.ml ltl_mc.ml ctls_mc.ml ctlskd_mc.ml main.ml -o mc.out

nusmv_wrapper: nusmv_wrapper.c
	gcc nusmv_wrapper.c -o nusmv_wrapper

tests_ltl:  common.ml logics.ml models.ml print.ml ltl_mc.ml tests_ltl.ml nusmv_wrapper
	ocamlc  common.ml logics.ml models.ml print.ml ltl_mc.ml tests_ltl.ml -o tests_ltl.out

debug: common.ml logics.ml models.ml print.ml ltl_mc.ml ctls_mc.ml ctlskd_mc.ml main.ml nusmv_wrapper.c
	ocamlc -g common.ml logics.ml models.ml print.ml ltl_mc.ml ctls_mc.ml ctlskd_mc.ml main.ml -o debug.out

clean:
	-rm *.cmi
	-rm *.cmo
	-rm *.out
	-rm ltl
	-rm output
	-rm nusmv_wrapper
