all:
	ghc --make -threaded -rtsopts naive_not_parallel.hs
	ghc --make -threaded -rtsopts naive_parallel_rpar.hs
	ghc --make -threaded -rtsopts four_parallel_par_monad.hs

clean:
	rm naive_not_parallel.o
	rm naive_not_parallel.hi
	rm naive_not_parallel
	rm naive_parallel_rpar.o
	rm naive_parallel_rpar.hi
	rm naive_parallel_rpar
	rm four_parallel_par_monad.o
	rm four_parallel_par_monad.hi
	rm four_parallel_par_monad
