program wave
    implicit none

    integer, parameter:: nmax=51, tmax=51
    integer:: i,j
    real, dimension(nmax, tmax):: u
    real, dimension(nmax):: x
    real, parameter:: dx=0.1, dt=0.05, c=1.0
    
    do i=1,nmax 
        x(i)=(i-1)*dx
    enddo

    ! initialize
    do i=1,21
        u(i,1) = 1.0
    enddo
    do i=22,nmax
        u(i,1) = 0.0
    enddo

    do j=1, tmax-1
        ! boundary condition
        u(1,j) = 2.0
        u(nmax,j) = 0.0

        do i=2,nmax-1
            u(i,j+1) = u(i,j) - c*dt/dx*(u(i,j) - u(i-1,j))
        enddo

        if (mod(j,10) == 0) then
            do i=1,nmax
                write(j, *) x(i), u(i,j)
            enddo
        end if
    enddo

end program