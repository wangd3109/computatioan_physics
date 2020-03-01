!! 1.3 finding roots

program main
      implicit none
      real :: x,tolerance,diff,dx,f_old
      integer :: iter

      x=0
      dx=20
      iter=0
      tolerance=1.e-06

      f_old=x**2-5
      
      10 continue
      iter=iter+1
      x=x+dx
      print *, iter,x,f_old,x**2-5
      if (f_old*(x**2-5) .le. 0) then
              x=x-dx
              dx=dx/2
      else
              f_old=x**2-5
      end if

      if (abs(dx) .gt. tolerance) goto 10


end program main      

!function f(x)
!        implicit none
!        real :: x
!        f=sqrt(x)-5
!end function
