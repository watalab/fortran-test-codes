program sum01

    implicit none
    
    integer i, wa
    integer, parameter:: n = 1000
    ! 1からnまでの整数の和を求める
    wa = 0
    do i=1, n
        wa = wa + i
    end do
    write(*,*) "sum=", wa

end program sum01
