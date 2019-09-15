SHELL = /bin/sh
RM = rm -f
LN = ln -fs

DUNE = dune

EXEC = lambda
MAIN_TARGET = main.bc
TARGETS = $(MAIN_TARGET) substitution_test.bc unification_test.bc

.PHONY: all $(TARGETS) $(EXEC) clean

all: $(TARGETS) $(EXEC)

$(TARGETS):
	$(DUNE) build $(addprefix src/,$(TARGETS))

$(EXEC): $(MAIN_TARGET)
	$(LN) _build/default/src/$< $(EXEC)

clean:
	$(DUNE) clean
	$(RM) $(EXEC)
