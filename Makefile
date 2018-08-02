SHELL = /bin/sh
RM = rm -f
LN = ln -fs

OCAMLBUILD = ocamlbuild -use-menhir
DEBUGFLAG = -g
OCAMLCFLAGS = -cflags $(DEBUGFLAG),-bin-annot,-w,+a-4
OCAMLLFLAGS = -lflags $(DEBUGFLAG)
MENHIRFLAGS = -yaccflags --explain,--strict

EXEC = lambda
MAIN_TARGET = main.byte
TARGETS = $(MAIN_TARGET) substitution_test.byte unification_test.byte

build = $(OCAMLBUILD) $(OCAMLCFLAGS) $(OCAMLLFLAGS) $(MENHIRFLAGS)
clean = $(OCAMLBUILD) -clean

.PHONY: all $(TARGETS) $(EXEC) clean

all: $(TARGETS) $(EXEC)

$(TARGETS):
	$(build) $@

$(EXEC): $(MAIN_TARGET)
	$(LN) $< $(EXEC)

clean:
	$(clean)
	$(RM) $(EXEC)
