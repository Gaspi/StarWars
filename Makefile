
MENHIR=-menhir "menhir --external-tokens Tokens"

PKG=graphics

SRC_DIRS=src

BINARIES=starwars

all: starwars

starwars:
	ocamlbuild -Is $(SRC_DIRS) $(MENHIR) -lib unix -pkg $(PKG) starwars.native

clean:
	ocamlbuild -clean

.PHONY: starwars clean
