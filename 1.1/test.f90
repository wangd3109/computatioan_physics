! from 1.1 Numerical differentiation
!! 3-point fomula
program main
      implicit none
      real :: h,x,f_prime,exact
      integer :: steps

      h=0.1
      x=1.
      exact=cos(x)
      f_prime=(sin(x+h)-sin(x-h))/(2*h)

      print*,exact,f_prime

end program

      

