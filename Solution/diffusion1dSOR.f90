program diffSOR
    implicit none

    integer, parameter:: nmax=51, tmax=12
    integer:: i,t,l, itermax=100
    real(8), dimension(nmax, tmax):: u
    real(8), dimension(nmax):: utemp
    real(8), dimension(nmax):: x
    real(8), parameter:: dx=0.2, dt=0.05, D=1.0, eps=1e-9, omega = 1.2
    real(8):: nu, res 

    nu = D*dt/dx**2
    print *, "diff no.=", nu
    
    do i=1,nmax 
        x(i)=-5.0+(i-1)*dx
    enddo

    ! initialize
    do i=1,nmax
        u(i,1) = 0.0
    enddo
    do i=25,27
        u(i,1) = 1.0
    enddo

    do t=1, tmax-1
        do i=2,nmax-1
            u(i, t+1) = u(i, t)
        enddo

        ! boundary condition
        u(1, t+1) = 0.0
        u(nmax, t+1) = 0.0

        ! Jaccobi
        do l=1,itermax
            do i=1,nmax
                utemp(i) = u(i, t+1) 
            enddo

            do i=2,nmax-1
                u(i, t+1) = (1.0-omega)*utemp(i) + omega*(u(i,t) +nu*utemp(i+1) + nu*u(i-1, t+1))/(1.0+2.0*nu)
                !u(i, t+1) = (u(i,t) +dd*utemp(i+1) + dd*u(i-1, t+1) )/(1.0+2.0*dd)
            enddo
            !print *, "u=", u(:,t+1)

            res = 0.0
            do i=2,nmax-1
                res = res + abs((utemp(i) - u(i,t+1))/(u(i,t+1)+1e-10))
            enddo
            !print *, "iter=",l, "res=", res

            if (res < eps) then
                print *, "t=", t+1, "iter=",l, "res=", res
                exit
            endif
            
        enddo

        if (mod(t,2) == 0) then
            do i=1,nmax
                write(t*1000, *) x(i), u(i,t+1)
            enddo
        end if
    enddo

end program