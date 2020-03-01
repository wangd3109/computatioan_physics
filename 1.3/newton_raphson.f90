!! 1.3 finding roots
! Newton-Raphson

program main
      implicit none
      real :: x_0,x_1,x,tolerance,diff,f_0,f,f_prime
      integer :: iter

      x_0=1
      iter=0
      tolerance=1.e-06

      10 continue
      iter=iter+1
      x_1=x_0-f(x_0)/f_prime(x_0)
      print *, 'iteration:',iter,'x_0:',x_0, 'x_1:',x_1, f(x_0), f(x_1)
      diff=x_1-x_0
      x_0=x_1
      if ( abs(diff) .gt. tolerance ) goto 10


end program main      

function f_prime(x)
        implicit none
        real :: h,x,f_prime,f

        h=0.001
        f_prime=1/(12*h)*(f(x-2*h)-8*f(x-h)+8*f(x+h)-f(x+2*h))
        
end function

function f(x)
        implicit none
        real :: f,x

        f=x**2-5
end function
