#FICHIERS ET COMMANDES PRINCIPALES
.PHONY: all run clean dependences doc

ocamlc := ocamlc#p -P a
ocamlopt := ocamlopt#p -P a

flags_opt := -I +unix -I +threads
flags_c := -I +unix -I +threads
extra_link_lib := unix threads

all_mli := $(wildcard src/*.mli)
all_ml := $(wildcard src/*.ml)

all : dependences main doc
main : debug.out main.out
run : doc debug.out main.out
	./main.out
#MODULES

main := main
test :=

build := $(main)

#CONSTRUCTION
main.out : $(addprefix bin/, $(build:=.cmx))
	$(ocamlopt) $(flags_opt) -o $@ $(addsuffix .cmxa, $(extra_link_lib)) $^

debug.out : $(addprefix debug/, $(build:=.cmo))
	$(ocamlc) $(flags_c) -g -o $@ $(addsuffix .cma, $(extra_link_lib)) $^

doc : doc/index.html

doc/index.html : $(patsubst src/%.mli, bin/%.cmi, $(all_mli))
	ocamldoc -html -charset UTF-8 -I bin -d doc $(all_mli)

dependences : Makefile.depends_bin Makefile.depends_debug

Makefile.depends_bin : $(all_mli) $(all_ml)
	ocamldep -I src $^ > $@
	sed -i 's/src/bin/g' $@

Makefile.depends_debug : $(all_mli) $(all_ml)
	ocamldep -I src $^ > $@
	sed -i 's/src/debug/g' $@

include Makefile.depends_bin
include Makefile.depends_debug


clean :
	rm -r bin/* debug/* doc/* Makefile.* *.out

bin/%.cmx : src/%.ml
	$(ocamlopt) $(flags_opt) -I bin -o $@ -c $<

bin/%.cmi : src/%.mli
	$(ocamlc) $(flags_c) -I bin -o $@ -c $<

bin/%.cmo : src/%.ml
	$(ocamlc) $(flags_c) -I bin -o $@ -c $<

debug/%.cmi : src/%.mli
	$(ocamlc) $(flags_c) -I debug -g -o $@ -c $<

debug/%.cmo : src/%.ml
	$(ocamlc) $(flags_c) -I debug -g -o $@ -c $<
