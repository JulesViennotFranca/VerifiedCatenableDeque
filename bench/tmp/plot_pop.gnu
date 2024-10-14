set terminal pngcairo
set output 'bench/result/pop.png'
set title 'Poping one time'
set xlabel 'length (number of elements)'
set ylabel 'time (ms)'
set key left top
plot 'bench/tmp/data_pop.txt' using 1:2 title 'List' with lines lt rgb '#E13ABD', \
     'bench/tmp/data_pop.txt' using 1:3 title 'Deque' with lines lt rgb '#F72500', \
     'bench/tmp/data_pop.txt' using 1:4 title 'Cadeque' with lines lt rgb '#5C7AFF', \
