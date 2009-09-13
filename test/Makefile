LIBS = str sqlite3

TESTS = $(wildcard *.ml)

all: bytecode opt

bytecode: $(TESTS:%.ml=%.bc)
opt: $(TESTS:%.ml=%.exe)

%.bc: %.ml
	ocamlc -w Ae -warn-error Ae -I .. str.cma sqlite3.cma $< -o $@

%.exe: %.ml
	ocamlopt -w Ae -warn-error Ae -I .. str.cmxa sqlite3.cmxa $< -o $@

clean:
	rm -f \
	  $(TESTS:%.ml=%.bc) \
	  $(TESTS:%.ml=%.exe) \
	  $(TESTS:%.ml=%.cmi) \
	  $(TESTS:%.ml=%.cmo) \
	  $(TESTS:%.ml=%.cmx) \
	  $(TESTS:%.ml=%.o) \
	  t
