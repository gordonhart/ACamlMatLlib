set terminal png

set autoscale 
unset log 
unset label
set xtic auto 
set ytic auto

set output 'lsq_cubicfittest.png'
set title "Least Squares Cubic Polynomial Interpolation"
set xlabel "x"
set ylabel "f(x)"
set autoscale x
set autoscale y
set key top left
plot "lsq_randpoints.dat" using 1:2 title "randomized" with points, \
	"lsq_points.dat" using 1:2 title "fit curve" with lines, \
	"lsq_realpoints.dat" using 1:2 title "actual curve" with lines



