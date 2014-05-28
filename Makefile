GHCOPTS		?= -O2 -rtsopts -threaded -W
BINFILES	?= TestHeap

all: $(BINFILES)

%: %.hs $(wildcard *.hs)
	ghc $(GHCOPTS) -o $@ --make $<

clean:
	-rm -f *.hi *.o *~

distclean: clean
	-rm -f $(BINFILES)

.PHONY: clean distclean
