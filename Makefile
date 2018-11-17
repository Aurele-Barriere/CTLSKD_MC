all: common.ml logics.ml models.ml
	ocamlc common.ml
	ocamlc logics.ml
	ocamlc models.ml

