set terminal pngcairo
set output 'bench/result/append.png'
set title 'Appending with itself'
set xlabel 'length (number of elements)'
set ylabel 'time (ms)'
set key left top
plot 'bench/tmp/data_append.txt' using 1:2 title 'List' with lines lt rgb '#E13ABD', \
     'bench/tmp/data_append.txt' using 1:3 title 'List rev' with lines lt rgb '#EE594D', \
     'bench/tmp/data_append.txt' using 1:4 title 'Cadeque' with lines lt rgb '#5C7AFF', \
