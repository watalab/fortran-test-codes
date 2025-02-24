program ODE
    implicit none
    integer:: i 
    real:: dt, k1, k2, k3, k4
    real, dimension(51):: ye, yr, t

    ye(1) = 1.0
    yr(1) = 1.0
    t(1) = 0.0
    dt = 0.1

    do i=1,50
        t(i+1) = i*dt
        ye(i+1) = ye(i) + dt*ye(i)

        k1 = yr(i)
        k2 = yr(i) + 0.5*dt*k1
        k3 = yr(i) + 0.5*dt*k2
        k4 = yr(i) + dt*k3
        yr(i+1) = yr(i) + 1.0/6.0*dt*(k1 + 2.0*k2 + 2.0*k3 + k4)
    enddo

    do i=1,50
        write(10, *) t(i), ye(i), yr(i)
    enddo

    
end program