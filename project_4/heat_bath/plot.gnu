#set terminal  postscript enhanced color font ",20"
set terminal pngcairo
set output '10.png'
set autoscale fix
set size ratio -1
set palette grey
#set palette rgbformulae 30,31,32
plot \
't10.dat' matrix with image
#'partial_dos.dat' u 1:2 w l lt 8 lw 3 lc 0 t "" ,\
