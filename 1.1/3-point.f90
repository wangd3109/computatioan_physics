! from 1.1 Numerical differentiation
!! 3-point fomula
program main
      implicit none
      real :: h,x,f_prime,exact
      integer :: steps

      print*, 'Enter value of H (.le. 0 to stop)'
      read*, h
      if (h .le. 0) stop
      x=1.
      exact=cos(x)
      f_prime=(sin(x+h)-sin(x-h))/(2*h)

      print*,exact,f_prime

end program

      

