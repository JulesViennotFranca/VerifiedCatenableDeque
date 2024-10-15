set terminal pngcairo
set output 'bench/result/append.png'
set title 'Concatenating 1000 times with itself'
set xlabel 'length (number of elements)'
set ylabel 'time (ms)'
set key left top
plot 'bench/tmp/data_append.txt' using 1:2 title 'List' with lines lt rgb '#DB3AE0', \
     'bench/tmp/data_append.txt' using 1:3 title 'List rev' with lines lt rgb '#EE594D', \
     'bench/tmp/data_append.txt' using 1:4 title 'Steque' with lines lt rgb '#BCC20A', \
     'bench/tmp/data_append.txt' using 1:5 title 'Cadeque' with lines lt rgb '#5C7AFF', \
