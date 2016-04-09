sources := $(wildcard *.hs)
name := rv-2-rka
doc := doc

trash := ${sources:.hs=.hi} ${sources:.hs=.o}

all:
	ghc --make -fno-warn-tabs -o $(name) $(sources)

.PHONY: doc
doc:
	haddock -h --odir=$(doc) $(sources)

clean:
	@- $(RM) $(name) $(trash) flp-fun-xmagam00.zip
pack: 
	zip flp-fun-xmagam00.zip rv-2-rka.hs README Makefile