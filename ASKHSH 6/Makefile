all:
	ghc --make -threaded -rtsopts parallel.hs
	ghc --make -threaded -rtsopts a.hs
	ghc --make -threaded -rtsopts ultra.hs

clean:
	rm a.o
	rm a.hi
	rm a
	rm parallel.o
	rm parallel.hi
	rm parallel
	rm ultra.o
	rm ultra.hi
	rm ultra
