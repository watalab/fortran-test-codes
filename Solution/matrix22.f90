program matmul2
    implicit none

    integer:: i, j, k
    integer, dimension(2,2) :: a, b, c

    do j=1,2
        do i=1,2
            c(i,j) = 0
            a(i,j) = (i-1)*2 + j
            b(i,j) = (1 + 2*(j-1))*i
            !print *, i, j, a(i,j), b(i,j)
        enddo
    enddo

    print *, "a=", a 
    print *, "b=", b

    do k=1,2
        do j=1,2
            do i=1,2
                c(i,j) = c(i,j) + a(i,k)*b(k,j)
            enddo
        enddo
    enddo

    print *, "c=", c
end program