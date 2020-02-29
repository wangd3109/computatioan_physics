!! 1.2 numerical quadrature
! simpson's rule

program main
      implicit none
      real :: h,x,integration,exact,x1,x2
      integer :: steps,i

      print*, 'Enter value of h (.le. 0 to stop)'
      read*, h

      x1=0.
      x2=1.
      steps=(x2-x1)/(2*h)
      exact=exp(1.)-1.

      do i=1,steps
      x=x1+h
      integration=integration+(h/3)*(exp(x+h)+4*exp(x)+exp(x-h))
      x1=x1+2*h
      end do

      print *, exact,integration

end program main

!subroutine f(x)
!        implicit none
!        real :: x
!
!        f
