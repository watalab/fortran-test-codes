program sum02

    implicit none
    
    integer :: wa, i
    integer, parameter :: n = 10 !項数
    integer, parameter :: r = 3 !公比

    wa = 0 
    do i=1, n
        wa = wa + r**(i-1)
    end do

    write(*,*) "numerical sum=", wa
    write(*,*) "analytical sum=", (r**n - 1)/(r-1)
end program sum02
