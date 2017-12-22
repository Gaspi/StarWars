
MENHIR = -menhir "menhir --external-tokens Tokens"

SRC_DIRS = src

BINARIES=starwars

all: starwars

starwars:
	ocamlbuild -Is $(SRC_DIRS) $(MENHIR) -lib unix starwars.native

clean:
	ocamlbuild -clean

.PHONY: starwars clean
