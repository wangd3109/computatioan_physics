program main
      implicit none
      real(8) :: lattice(16,16),jex,b,randomx,randomy,h1,h2,tmp,t,summationh,summationm,averageh,averagem,mag,start,finish,m2,m4,u
	real(8) :: m2_avg,m4_avg,e,m,e_old,m_old,jex2
      integer ::i,j,s1,s2,cont,steps,grid,k
      character (len=100) :: filename,datafile
      real,external :: exchange,efield,r,p,exchange2

	open(unit=1,file='data.dat') 
      write(1,*) "Temperature       Hamiltonian       Magnetization       4th_order_cumulant"
      do k=5,200,1
!	call cpu_time(start)
!	k=5
      t=0.05*k      ! temperature
      jex=1.      ! exchange parameter
      jex2=-1.
      b=0.        ! electron field
      cont=0
      grid=16
	steps=1000*(grid**2)
	m4=0
	m2=0
      e_old=e
      m_old=m

	
	!初始化lattice
      do i=1,grid
      do j=1,grid
      lattice(i,j)=1
      end do
      end do
	
	!对lattice进行格点数次的翻转
      call random_seed()
      do i=1,grid**2
      call random_number(randomx)
      call random_number(randomy)
      randomx=randomx*grid+1
      randomy=randomy*grid+1

      lattice(int(randomx),int(randomy))=-1*lattice(int(randomx),int(randomy))
      end do                            ! now we have the lattice
	
	!检查LATTICE是否正确
!      print*,lattice

	!subroutine,处理得到磁矩？h1（哈密顿量）？对初始化并翻转后的lattice计算此时的性质（磁性，哈密顿量）
      call hamil(grid,jex,b,lattice,mag,h1)
      summationh=h1
!      mag=abs(mag)
      summationm=mag
      cont=1
	h1=h1/(grid**2)
	mag=mag/(grid**2)
!      write(k,*) "steps:",cont,"Hamiltonian:",h1,"Magnetization:",mag
      
	!随机翻转一个格点上的磁矩，并计算磁化强度，哈密顿量，循环指定步数
      10 continue
      call random_number(randomx)
      call random_number(randomy)
      randomy=randomy*grid+1
      randomx=randomx*grid+1
      lattice(int(randomx),int(randomy))=-1*lattice(int(randomx),int(randomy))

      call hamil(grid,jex,b,lattice,mag,h2)
!      mag=abs(mag)

	!根据能量判定，如果反转后能量变小或保持不变，则保留；如果变大则生成一个随机数，如果随机数小于r，保留翻转，否则，退回
      if (h2 .le. h1) then
              !cont=cont+1
!              summationh=summationh+h2
!              summationm=summationm+mag
              h1=h2
      else 
              call random_number(tmp)
              if (tmp .lt. r(h1,h2,t)) then
              !        cont=cont+1
!                      summationh=summationh+h2
!                      summationm=summationm+mag
				h1=h2
              else
                      lattice(int(randomx),int(randomy))=-1*lattice(int(randomx),int(randomy))
				call hamil(grid,jex,b,lattice,mag,h2)
              end if
      end if
     
	cont=cont+1
	h2=h2/(grid**2)
	mag=mag/(grid**2)
!      print*,cont,"Hamiltonian:",h2,"magnetization:",mag
!      write(k,*) "steps:",cont,"Hamiltonian:",h2,"Magnetization:",mag,"Mag**4:",mag**4,"Mag**2:",mag**2
	if (cont .gt. steps/2) then
		m4=m4+mag**4
		m2=m2+mag**2
		e=h2+e
		m=mag+m
	else
		m4=m4
		m2=m2
		e=e
	end if
      if (cont .lt. steps) goto 10

	m4_avg=m4/(steps/2)
	m2_avg=m2/(steps/2)
	u=1-(m4_avg)/(3*((m2_avg)**2))
	e=e/(steps/2)
	m=m/(steps/2)
      write(1,'(f10.6,4x,f10.6,4x,f10.6,4x,f10.6,4x,f10.6,4x,f10.6)') t,e,m,u,e-e_old,m-m_old
!	write(1,*) "Temperature:",t,"Hamiltonian:",e,"Magnetization:",m,"4th_order_cumulant:",u

	!决定删除这些，把做平均的步骤放到了之前       
!      averageh=summationh/steps
!      averageh=averageh/(grid**2)
!      averagem=summationm/steps
!	averagem=averagem/(grid**2)
!	call cpu_time(finish)
!      print* ,"temperature:",t,"electronic_field:", b,averageh,averagem,"cpu_time:",finish-start

      end do
	close(unit=1)

end program main

real function exchange(s0,su,sd,sl,sr)
        implicit none
        integer :: s0,su,sd,sl,sr
        exchange=s0*(su+sd+sl+sr)
end function

real function exchange2(s0,sul,sur,sdl,sdr)
        implicit none
        integer :: s0,sul,sur,sdl,sdr
        exchange2=s0*(sul+sur+sdl+sdr)
end function

!需要这个吗？？
real function efield(a)
        implicit none
        integer :: a
        efield=a
end function

!
real function r(h1,h2,t)
        implicit none
        real(8) :: h1,h2,t
        r=exp((h1-h2)/t)
end function

real function p(h2,t)
	implicit none
	real(8) :: h2,t
	p=exp(-h2/t)
end function

subroutine hamil(grid,jex,b,lattice,mag,h)
        implicit none
        real(8) :: lattice(16,16),h,jex,b,summation1,mag        !这里x没有问题？
        real(8) :: summation2,jex2
        integer :: i,j,s0,su,sd,sl,sr,grid
        integer :: sul,sur,sdl,sdr                            !second nearest neighbour
        real,external :: exchange, efield, exchange2

        summation1=0
        mag=0
        h=0
       

	!计算第一项，交换关联
        do i=1,grid                                           !lattice
        do j=1,grid
        s0=lattice(i,j)
        if (i .eq. 1) then
                if (j .eq. 1) then
                        su=lattice(i,j+1)
                        sd=lattice(i,grid)
                        sl=lattice(grid,j)
                        sr=lattice(i+1,j)
                        sul=lattice(grid,j+1)
                        sur=lattice(i+1,j+1)
                        sdl=lattice(grid,grid)
                        sdr=lattice(i+1,grid)
                else if (j .gt. 1 .and. j .lt. grid) then
                        su=lattice(i,j+1)
                        sd=lattice(i,j-1)
                        sl=lattice(grid,j)
                        sr=lattice(i+1,j)
                        sul=lattice(grid,j+1)
                        sur=lattice(i+1,j+1)
                        sdl=lattice(grid,j-1)
                        sdr=lattice(i+1,j-1)
                else if (j .eq. grid) then
                        su=lattice(i,1)
                        sd=lattice(i,j-1)
                        sl=lattice(grid,j)
                        sr=lattice(i+1,j)
                        sul=lattice(grid,1)
                        sur=lattice(i+1,1)
                        sdl=lattice(grid,j-1)
                        sdr=lattice(i+1,j-1)
                end if
        else if (i .gt. 1 .and. i .lt. grid) then
                if (j .eq. 1) then
                        su=lattice(i,j+1)
                        sd=lattice(i,grid)
                        sl=lattice(i-1,j)
                        sr=lattice(i+1,j)
                        sul=lattice(i-1,j+1)
                        sur=lattice(i+1,j+1)
                        sdl=lattice(i-1,grid)
                        sdr=lattice(i+1,grid)
                else if (j .gt. 1 .and. j .lt. grid) then
                        su=lattice(i,j+1)
                        sd=lattice(i,j-1)
                        sl=lattice(i-1,j)
                        sr=lattice(i+1,j)
                        sul=lattice(i-1,j+1)
                        sur=lattice(i+1,j+1)
                        sdl=lattice(i-1,j-1)
                        sdr=lattice(i+1,j-1)
                else if (j .eq. grid) then
                        su=lattice(i,1)
                        sd=lattice(i,j-1)
                        sl=lattice(i-1,j)
                        sr=lattice(i+1,j)
                        sul=lattice(i-1,1)
                        sur=lattice(i+1,1)
                        sdl=lattice(i-1,j-1)
                        sdr=lattice(i+1,j-1)
                end if
        else if (i .eq. grid) then
                if ( j .eq. 1) then
                        su=lattice(i,j+1)
                        sd=lattice(i,grid)
                        sl=lattice(i-1,j)
                        sr=lattice(1,j)
                        sul=lattice(i-1,j+1)
                        sur=lattice(1,j+1)
                        sdl=lattice(i-1,grid)
                        sdr=lattice(1,grid)
                else if (j .gt. 1 .and. j .lt. grid) then
                        su=lattice(i,j+1)
                        sd=lattice(i,j-1)
                        sl=lattice(i-1,j)
                        sr=lattice(1,j)
                        sul=lattice(i-1,j+1)
                        sur=lattice(1,j+1)
                        sdl=lattice(i-1,j-1)
                        sdr=lattice(1,j-1)
                else if (j .eq. grid) then
                        su=lattice(i,1)
                        sd=lattice(i,j-1)
                        sl=lattice(i-1,j)
                        sr=lattice(1,j)
                        sul=lattice(i-1,1)
                        sur=lattice(1,1)
                        sdl=lattice(i-1,j-1)
                        sdr=lattice(1,j-1)
                end if
        end if

        summation1=summation1+exchange(s0,su,sd,sl,sr)
        summation2=summation2+exchange2(s0,sul,sur,sdl,sdr)
        end do
        end do

	!计算第二项，磁场对能量的贡献，对spin求和
        do i=1,grid
        do j=1,grid
        s0=lattice(i,j)
        mag=mag+efield(s0)
        end do
        end do

        summation1=summation1/2
        summation2=summation2/2
        h=-(jex*summation1+jex2*summation2)-b*mag
        
end subroutine hamil

