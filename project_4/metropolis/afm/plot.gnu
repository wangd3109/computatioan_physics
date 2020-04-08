#set terminal  postscript enhanced color font ",20"
set terminal pngcairo
set output '5.png'
unset xtics
unset colorbox
set autoscale fix
set size ratio -1
set palette grey
#set palette rgbformulae 30,31,32
plot \
't5.dat' matrix with image
#'partial_dos.dat' u 1:2 w l lt 8 lw 3 lc 0 t "" ,\
