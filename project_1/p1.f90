program main
      implicit none
      real :: b0, db, b, rmax, pi, V, E, r, term1
      real :: theta_1, theta_2, g
      real :: inte
      integer :: i
      
      b0 = 0.1
      db = 0.5
      rmax = 10
      V = 5
      E = 10
      pi = 4*atan(1.0)

!
      b=b0
      do i = 1,100,1
      b=b0+db*i
 
      ! for the first term
!      call integration(b,rmax,g(x,b))
!      term1 = inte
!      print *, term1

      ! for the second term
!      g(x) = (1/r**2)*sqrt(1-(b/r)**2-V/E)
!      call integration(rmin,rmax,g(x))
!      term2 = inte

!      theta = 2*b(term1-term2)    

      call analytical(b,rmax,v,e)
      call g_rmin(b,v,e,rmax)

!      print*, '0',b, rmax, rmin
      end do
      call integration(b,rmax)

!      print*, "b:",b,"rmax:",rmax



     
end program main


subroutine integration(x1,x2)
        implicit none
        real :: b,rmax,h,x1,x2,inte
        integer :: steps, i

        h = 0.001
        x1 = b
        x2 = rmax
        steps = (x2-x1)/(2*h)
        inte = 0
        
        print *, "x1:",x1,"x2:",x2,'steps', steps
        do i = 1,steps
        print *, "iteration:",i, inte
        inte =inte+(h/3)*((1/x1**2)*sqrt(1-(b/x1)**2)+4*(1/(x1+h)**2)*sqrt(1-(b/(x1+h))**2)+(1/(x1+2*h)**2)*sqrt(1-(b/(x1+2*h))**2))
        x1 = x1+2*h
        end do

        print *, "term1:",inte
end subroutine

!function g(x)
!        implicit none
!        real :: x,b
!        real :: g
!            
!        g = (1/x**2)*sqrt(1-(b/x)**2)
!end function

!!! to get the rmin
subroutine g_rmin(b,v,e,rmax)
        implicit none
        real :: b,v,e,rmax,rmin
        real :: tolerance, r0, r1, r2, diff
        real :: f
        integer :: iter
!
        r0 = rmax
        r1 = r0 - 0.1
        iter = 0
        tolerance = 1.e-06
      
        10 continue
        iter = iter+1
        r2=r1-f(r1,b,v,e)*(r1-r0)/(f(r1,b,v,e)-f(r0,b,v,e))
!        print *, "iteration:", iter, "r0:", r0, "r1:", r1, f(r0,b,v,e), f(r1,b,v,e)
        diff = r2-r1
        r0 = r1
        r1 = r2
        if ( abs(diff) .gt. tolerance ) goto 10

        print *,"rmin:",r1,sqrt((e*b**2)/(e-v))
end subroutine g_rmin

function f(x,b,v,e)
        implicit none
        real :: b, v, e
        real :: x
        real :: f

        f=1-(b/x)**2-V/E

end function f


!!! to get the analytical solution
subroutine analytical(b, rmax, V, E)
        implicit none
        real :: b, rmax, V, E 
        real :: theta_1, theta_2

        if ( e < v) then
                print *, "analytical, b:",b,"theta:", theta_1(b,rmax)
        else
                print *, "analytical, b:",b,"theta:", theta_2(b,rmax,V,E)
        end if
end subroutine analytical
 
function theta_1(b,rmax)
        implicit none
        real :: b, rmax, pi
        real :: theta_1

        theta_1 = pi-2*asin(b/rmax)

end function

function theta_2(b,rmax,V,E)
        implicit none
        real :: b, rmax, V, E, pi
        real :: theta_2

        theta_2 = 2*(asin(b/(rmax*sqrt(1-V/E)))) - asin(b/rmax)

end function

        
