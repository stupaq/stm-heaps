GHCOPTS		?= -O2 -rtsopts -threaded -W -Wall -fno-warn-unused-imports
BINFILES	?= TestHeap TestSort

all: $(BINFILES)
TestSort: TestHeap

%: %.hs $(wildcard *.hs)
	ghc $(GHCOPTS) $(CPPFLAGS) -o $@ --make $<

clean:
	-rm -f *.hi *.o *~

distclean: clean
	-rm -f $(BINFILES)

.PHONY: clean distclean
