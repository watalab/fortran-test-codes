program diff2dSOR
    implicit none

    integer, parameter:: nmax=51, tmax=100
    integer:: i,j,t
    real(8), dimension(nmax, nmax, tmax):: u
    real(8), dimension(nmax, nmax):: utemp
    real(8), dimension(nmax, nmax):: x, y
    real(8), parameter:: dx=0.1, dt=0.05, D=0.10, eps=1e-9, omega = 1.2
    real(8):: nu

    nu = D*dt/dx**2
    print *, "diff no.=", nu
    
    do j=1, nmax
        do i=1,nmax 
            x(i, j)=-5.0+(i-1)*dx
            y(i, j)=-5.0+(j-1)*dx
        enddo
    enddo

    ! initialize
    do j=1,nmax
        do i=1,nmax
            u(i,j,1) = 0.0
        enddo
    enddo
    do j=25,27
        do i=25,27
            u(i,j,1) = 1.0
        enddo
    enddo
    do j=1,nmax
        do i=1,nmax
            write(10000, *) x(i,j), y(i,j), u(i,j,1)
        enddo
        write(10000, *)
    enddo

    do t=1, tmax-1
        do j=1,nmax-1
            do i=2,nmax-1
                u(i,j, t+1) = u(i,j, t)
            enddo
        enddo

        ! boundary condition
        do j=1,nmax
            u(1,j, t+1) = 0.0
            u(nmax,j, t+1) = 0.0
        enddo
        do i=1,nmax
            u(i,1,t+1) = 0.0
            u(i,nmax, t+1) = 0.0
        enddo

        ! SOR
        call sor(u(:,:,t+1), u(:,:, t), utemp, nu, nmax, eps, omega)

        if (mod(t,10) == 0) then
            do j=1,nmax
                do i=1,nmax
                    write(t*10000, *) x(i,j), y(i,j), u(i,j,t+1)
                enddo
                write(t*10000, *)
            enddo
        end if
    enddo

end program

subroutine sor(u, uold, utemp, nu, nmax, eps, omega)
    implicit none
    integer:: nmax

    real(8), dimension(nmax, nmax):: u, uold
    real(8), dimension(nmax, nmax):: utemp
    real(8):: eps, omega
    real(8):: nu, res 
    integer:: i,j,l, itermax=100

    do l=1,itermax
        do j=1,nmax
            do i=1,nmax
                utemp(i,j) = u(i,j) 
            enddo
        enddo

        do j=2,nmax-1
            do i=2,nmax-1
                u(i,j) = (1.0-omega)*utemp(i,j) + & 
                omega*(uold(i,j) +nu*utemp(i+1,j) + nu*u(i-1,j) + nu*u(i, j-1) + nu*utemp(i,j+1))/(1.0+4.0*nu)
                !u(i, t+1) = (u(i,t) +dd*utemp(i+1) + dd*u(i-1, t+1) )/(1.0+2.0*dd)
            enddo
        enddo
            !print *, "u=", u(:,t+1)

        res = 0.0
        do j=2,nmax-1
            do i=2,nmax-1
                res = res + abs((utemp(i,j) - u(i,j))/(u(i,j)+1e-10))
            enddo
        enddo
        !print *, "iter=",l, "res=", res

        if (res < eps) then
            print *, "iter=",l, "res=", res
            exit
        endif
            
    enddo
end 