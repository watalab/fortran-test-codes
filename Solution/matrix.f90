program matmuln
    implicit none

    integer:: i, j, k
    integer, parameter:: n = 500
    real(8), dimension(n,n) :: a, b, c

    do j=1,n
        do i=1,n
            c(i,j) = 0.0
            a(i,j) = -2.0*(i-1) + j
            b(i,j) = (1 + 2.0*(j-1))
            !print *, i, j, a(i,j), b(i,j)
        enddo
    enddo

    !print *, "a=", a 
    !print *, "b=", b

    !$omp parallel
    !$OMP DO
    do k=1,n
        do j=1,n
            do i=1,n
                c(i,j) = c(i,j) + a(i,k)*b(k,j)
            enddo
        enddo
    enddo
    !$OMP end do
    !$omp end parallel

    !print *, "c=", c
end program