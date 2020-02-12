echo "Time of single core:"
time (cat samples.txt) | ./naive_not_parallel

echo "Time of parallel:"
time (cat samples.txt) | ./naive_parallel_rpar +RTS -N4

echo "Time of ultra:"
time (cat samples.txt) | ./four_parallel_par_monad +RTS -N8
