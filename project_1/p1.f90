program main
      implicit none
      real :: b0, db, b, rmax, pi, V, E
      real :: theta_1, theta_2
      integer :: i
      
      b0=0.1
      db=0.5
      rmax = 10
      V=5
      E=10
!
      do i = 1,100,1
      b=b0+db*i
      if ( e < v) then
              print *, b, theta_1(b,rmax)
      else
              print *, b, theta_2(b,rmax,V,E)
      end if
      end do

end program main

function theta_1(b,rmax)
        implicit none
        real :: b, rmax, pi
        real :: theta_1

        pi = 4*atan(1.0)
        theta_1 = pi-2*asin(b/rmax)

end function

function theta_2(b,rmax,V,E)
        implicit none
        real :: b, rmax, V, E, pi
        real :: theta_2

        pi = 4*atan(1.0) !try to put this pi at the begining of the code
        theta_2 = 2*(asin(b/(rmax*sqrt(1-V/E)))) - asin(b/rmax)

end function

        
