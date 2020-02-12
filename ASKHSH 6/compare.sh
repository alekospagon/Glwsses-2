echo "Time of single core:"
time (cat samples.txt) | ./a

echo "Time of parallel:"
time (cat samples.txt) | ./parallel +RTS -N4

echo "Time of ultra:"
time (cat samples.txt) | ./ultra +RTS -N8
