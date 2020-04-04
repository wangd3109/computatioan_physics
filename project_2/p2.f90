program main
      implicit none
      real :: b0, db, b, rmax, rmin, pi, V, E, r
      real :: V0, a
      real :: theta_1, theta_2, g
      ! for the integrations
      real :: theta_a
      real :: term1, term2, h, t1, t2, theta
      integer :: i,j,k, steps
      
      b0 = 1
      db = 0.1
      rmax = 10
      a=rmax/3.
      V0 = 10 
      E =  5
      pi = 4*atan(1.0)

      b = b0

      open(10,file='output.dat')
      write(10,*), "#   b      analytical      numerical"

      do i = 0,100,1
      b=b0+db*i
      V=4*V0*((a/b)**2-(a/b)**6)
!      print*, V0,V

      theta = 0
      rmin = sqrt((e*b**2)/(e-v))
!      print *,'rmin:',rmin


!! the analytical solution 
!      call analytical(b,rmax,v,e,pi)
        if ( e < v) then
                theta_a = theta_1(b,rmax,pi)
        else
                theta_a = theta_2(b,rmax,V,E,pi)
        end if
   

!! the numerical solution
      h = 0.0001

      !! THE FIRST TERM
      steps = (rmax-b)/(2*h)
      term1 = 0
      r=b+h
      do j = 1, steps         ! do integration for the first term
      term1=term1+(h/3)*(t1(r,b)+4*t1(r+h,b)+t1(r+2*h,b))
      r = r+2*h
      end do
!      print *, "b",b,'rmax:',rmax,'term1:',term1

      !! THE SECOND TERM

!      print *,  t2(2.,b,V,E)
      steps = (rmax-rmin)/(2*h)
      term2 = 0
      r=rmin+h
      do k = 1, steps
      term2 = term2+(h/3)*(t2(r,b,V,E)+4*t2(r+h,b,V,E)+t2(r+2*h,b,V,E))
      r = r+2*h
      end do
!      print *, b,'rmin:',rmin,'rmax:',rmax,'term2:', term2

      theta = 2*b*(term1-term2)
      write(10,'(f5.1,4x,f10.6,4x,f10.6)'), b, theta_a, theta !theta*sqrt(E/(4*V0))*b/rmin
      print *,b, theta_a,  theta

      end do

     
end program main

function v(r,V0,a)
        implicit none
        real :: r, V0, a, v

        V=4*V0*((a/r)**2-(a/r)**6)
end function

function t1(r,b)
        implicit none
        real :: r,b,t1

        t1 = (1/r**2)*(1-(b/r)**2)**(-1/2.)
end function

function t2(r,b,V,E)
        implicit none
        real :: r,b,V,E
        real :: t2

        t2 = (1./r**2)*(1.-(b/r)**2.-V/E)**(-1/2.) 
end function


function f(x,b,v,e)
        implicit none
        real :: b, v, e
        real :: x
        real :: f

        f=1-(b/x)**2-V/E

end function f


!!! to get the analytical solution
subroutine analytical(b, rmax, V, E, pi)
        implicit none
        real :: b, rmax, V, E ,pi
        real :: theta_1, theta_2

        if ( e < v) then
                print *, "analytical solution1:",b, theta_1(b,rmax,pi)
        else
                print *, "analytical solution2:",b, theta_2(b,rmax,V,E,pi)
        end if
end subroutine analytical
 
function theta_1(b,rmax,pi)
        implicit none
        real :: b, rmax, pi
        real :: theta_1

        theta_1 = pi-2*asin(b/rmax)

end function

function theta_2(b,rmax,V,E,pi)
        implicit none
        real :: b, rmax, V, E, pi
        real :: theta_2

        theta_2 = 2*( asin(b/(rmax*sqrt(1-v/e))) -asin(b/rmax) )

end function

        
