program diff1d
    implicit none

    integer, parameter:: nmax=51, tmax=51
    integer:: i,j
    real(8), dimension(nmax, tmax):: u
    real(8), dimension(nmax):: x
    real(8), parameter:: dx=0.2, dt=0.01, D=1.0
    
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

    do j=1, tmax-1
        ! boundary condition
        u(1,j) = 0.0
        u(nmax,j) = 0.0

        do i=2,nmax-1
            u(i,j+1) = u(i,j) + D*dt/dx**2*(u(i+1,j) - 2.0*u(i,j) + u(i-1,j))
        enddo

        if (mod(j,10) == 0) then
            do i=1,nmax
                write(j, *) x(i), u(i,j+1)
            enddo
        end if
    enddo

end program