program FizzBuzz

    implicit none

    integer :: i 

    do i=1,30
        if (mod(i, 15)== 0) then
            print *, i, "FizzBuzz"
        else if (mod(i, 3) ==0) then
            print *, i, "Fizz"
        else if (mod(i, 5) ==0) then
            print *, i, "Buzz"
        else
            print *, i
        end if
    enddo    

end program FizzBuzz
