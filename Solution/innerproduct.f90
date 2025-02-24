program innerproduct
    implicit none

    integer :: i 

    real,dimension(3) :: a, b, outer
    real :: inner

    a(1) = 1.0
    a(2) = 2.0
    a(3) = 3.0
    b(1) = -1.0
    b(2) = 2.0
    b(3) = 5.0

    inner = 0.0
    do i = 1,3
        inner = inner + a(i)*b(i)
    enddo
    print *, "inner product=", inner

    outer(1) = a(2)*b(3) - a(3)*b(2)
    outer(2) = a(3)*b(1) - a(1)*b(3)
    outer(3) = a(1)*b(2) - a(2)*b(1)

    print *, "outer product=", outer
end program