module vars
    implicit none

    integer, parameter:: jmax=1001, nmax=1500
    real(8), parameter:: dt=2d-4, xmax=1.0d0, xmin=0.0d0, gamma=1.4d0
    real(8), parameter:: dx=(xmax-xmin)/(jmax-1), eps=0.0
    real(8), parameter:: pl=1.0d0, pr=0.1d0, rhol=1.0d0, rhor=0.1d00, ul=0.0d0, ur=0.0d0

    real(8), dimension(jmax):: x, p
    real(8), dimension(jmax, 3):: Q, E
end

program main
    use vars
    implicit none
    integer:: n,j

    call init()
    do n=1,nmax
        !call stepMacCormack()
        call step()
        if (mod(n, 100) == 0) then
            write(n+10, *) "# x, rho, u, p, T"
            do j=1,jmax
                write(n+10, *) x(j), Q(j,1), Q(j,2)/Q(j,1), (gamma-1d0)*(Q(j,3)-0.5d0*Q(j,2)**2/Q(j,1)), (gamma-1d0)*(Q(j,3)-0.5d0*Q(j,2)**2/Q(j,1))/Q(j,1)
            enddo
        endif
    enddo
end program

subroutine init()
    use vars
    implicit none
    integer:: j

    do j=1,jmax
        x(j) = dx*(j-1)
    enddo

    do j=1, (jmax-1)/2
        Q(j,1) = rhol
        Q(j,2) = rhol*ul
        Q(j,3) = pl/(gamma-1.0) + 0.5*rhol*ul**2
    enddo

    do j=(jmax-1)/2+1,jmax
        Q(j,1) = rhor
        Q(j,2) = rhor*ur
        Q(j,3) = pr/(gamma-1.0) + 0.5*rhor*ur**2
    enddo
end

subroutine calcFlux(Qq)
    use vars
    implicit none
    integer:: j
    real(8), dimension(jmax,3):: Qq

    do j=1,jmax
        E(j,1) = Qq(j,2)
        E(j,2) = (gamma - 1.0d0)*Qq(j,3) + (3d0-gamma)/2d0*Qq(j,2)**2/Qq(j,1)
        E(j,3) = (gamma*Qq(j,3) - 0.5d0*(gamma-1d0)*Qq(j,2)**2/Qq(j,1))*Qq(j,2)/Qq(j,1)
    enddo
end

subroutine step()
    use vars
    implicit none
    integer:: j, d
    real(8), dimension(jmax,3):: Qs
    real(8):: k, d1, d2

    call calcFlux(Q)
    do d=1,3
        do j=1,jmax
            Qs(j, d) = Q(j,d) 
        enddo
    enddo

    do d=1,3
        do j=1,jmax-1
            Qs(j, d) = 0.5d0*(Q(j+1,d)+Q(j,d)) - 0.5d0*dt/dx*(E(j+1,d) - E(j,d))
        enddo
    enddo
    print *, Qs(jmax-1, 2), Qs(jmax-1, 3)

    call calcFlux(Qs)
    do d=1,3
        do j=2,jmax-1
            Q(j, d) = Q(j,d) - dt/dx*(E(j,d) - E(j-1,d))
        enddo
    enddo

    print *, Q(jmax-1, 2), Q(jmax-1, 3)
    do d=1,3
        do j=1,jmax
            Qs(j, d) = Q(j,d) 
        enddo
    enddo

    do d=1,3
        do j=2,jmax-1
            d1 = Qs(j-1,d) -2d0*Qs(j,d) + Qs(j+1,d)
            d2 = Qs(j-1,d) +2d0*Qs(j,d) + Qs(j+1,d)
            k = eps*abs(d1)/(abs(d2)+1d-10)
            Q(j,d) = Q(j,d) + k*d1
        enddo
    enddo
    print *, Q(jmax-1, 2), Q(jmax-1, 3)

end

subroutine stepMacCormack()
    use vars
    implicit none
    integer:: j, d
    real(8), dimension(jmax,3):: Qs
    real(8):: k, d1, d2

    call calcFlux(Q)
    do d=1,3
        do j=1,jmax
            Qs(j, d) = Q(j,d) 
        enddo
    enddo
    do d=1,3
        do j=2,jmax-1
            Qs(j, d) = Q(j,d) - dt/dx*(E(j,d) - E(j-1,d))
        enddo
    enddo
    print *, Qs(5, 2), Qs(5, 3)
    
    call calcFlux(Qs)
    do d=1,3
        do j=2,jmax-1
            Q(j, d) = 0.5d0*(Q(j,d)+Qs(j,d)) - 0.5d0*dt/dx*(E(j+1,d) - E(j,d))
        enddo
    enddo
    print *, Q(5, 2), Q(5, 3)

    do d=1,3
        do j=1,jmax
            Qs(j, d) = Q(j,d) 
        enddo
    enddo

    do d=1,3
        do j=2,jmax-1
            d1 = Qs(j-1,d) -2d0*Qs(j,d) + Qs(j+1,d)
            d2 = Qs(j-1,d) +2d0*Qs(j,d) + Qs(j+1,d)
            k = eps*abs(d1)/(abs(d2)+1d-10)
            Q(j,d) = Q(j,d) + k*d1
        enddo
    enddo
    print *, Q(5, 2), Q(5, 3)

end
