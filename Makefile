CC=ghc
SRCDIR=./src
ONAME=xmagam00
HADDOCK=haddock
HTITLE=rv-2-rka
HPARAMS= -h -U -w --title $(HTITLE) -p ../doc/prologue

build:	
	cd $(SRCDIR);$(CC) --make Main -o ../$(ONAME)
.PHONY: clean
clean:  
	rm -f $(SRCDIR)/*.o $(SRCDIR)/*.hi ./$(ONAME)