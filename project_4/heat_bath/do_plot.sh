for i in `seq 5 5 500`
do
	  cp plot.gnu plotnow.gnu
	  sed -i '' '9s/t10.dat/t'$i'.dat/' plotnow.gnu
	  sed -i '' '3s/10.png/'$i'.png/' plotnow.gnu
	  gnuplot plotnow.gnu
done
