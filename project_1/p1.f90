program main
      implicit none
      real :: b0, db, b, rmax, rmin, pi, V, E, r
      real :: theta_1, theta_2, g
      ! for the integrations
      real :: term1, term2, h, t1, t2, theta
      integer :: i,j,k, steps
      
      b0 = 2
      db = 0.1
      rmax = 10
      V = 5
      E = 10
      pi = 4*atan(1.0)

      b = b0
      rmin = sqrt((e*b**2)/(e-v))
      print *,'rmin:',rmin

!
      do i = 0,0,1
      b=b0+db*i

!! the analytical solution 
      call analytical(b,rmax,v,e,pi)

!! the numerical solution
      h = 0.0001

      !! THE FIRST TERM
      steps = (rmax-b)/(2*h)
      term1 = 0
      r=b+h
      print *, b,rmax,steps
      do j = 1, steps         ! do integration for the first term
      ! checking print
!      print *, r, b
!      print *, 'mark',(1/r**2)*(1-(b/r)**2)**(1/2.),(1-(b/r)**2.)
!      print *, t1(r,b), t1(r+h,b), t1(r+2*h,b)
      term1=term1+(h/3)*(t1(r,b)+4*t1(r+h,b)+t1(r+2*h,b))
      r = r+2*h
      end do

      print *, "b",b,'rmax:',rmax,'term1:',term1

      !! THE SECOND TERM

!      print *,  t2(2.,b,V,E)
      steps = (rmax-rmin)/(2*h)
      term2 = 0
      r=rmin+h
      do k = 1, steps
!      print *, 'r:',r,'b:',b,'v:',v,'e:',e
!      print *, t2(r,b,v,e)
      term2 = term2+(h/3)*(t2(r,b,V,E)+4*t2(r+h,b,V,E)+t2(r+2*h,b,V,E))
      r = r+2*h
      end do
      print *, 'rmin:',rmin,'rmax:',rmax,'term2:', term2

      theta = 2*b*(term1-term2)
      print *, 'numerical  solution:', b, theta

      end do

     
end program main

function t1(r,b)
        implicit none
        real :: r,b,t1

        t1 = (1/r**2)*(1-(b/r)**2)**(-1/2.)
end function

function t2(r,b,V,E)
        implicit none
        real :: r,b,V,E
        real :: t2

        t2 = (1/r**2)*(1-(b/r)**2-V/E)**(-1/2.) 
end function


!subroutine integration(x1,x2)
!        implicit none
!        real :: b,rmax,h,x1,x2,inte
!        integer :: steps, i

!        h = 0.001
!        x1 = b
!        x2 = rmax
!        steps = (x2-x1)/(2*h)
!        inte = 0
        
!        print *, "x1:",x1,"x2:",x2,'steps', steps
!        do i = 1,steps
!        print *, "iteration:",i, inte
!        inte =inte+(h/3)*((1/x1**2)*sqrt(1-(b/x1)**2)+4*(1/(x1+h)**2)*sqrt(1-(b/(x1+h))**2)+(1/(x1+2*h)**2)*sqrt(1-(b/(x1+2*h))**2))
!        x1 = x1+2*h
!        end do

!        print *, "term1:",inte
!end subroutine

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
!                print *, v, e, b, rmax
        else
                print *, "analytical solution2:",b, theta_2(b,rmax,V,E,pi)
!                print *, b, rmax
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
!        theta_2 = asin(1.)/(2*pi)*360
!        theta_2 = pi
!        theta_2 = asin(2/(10*sqrt(0.5)))*2/pi*360
!        theta_2 = 2+b+rmax+v+e

end function

        
