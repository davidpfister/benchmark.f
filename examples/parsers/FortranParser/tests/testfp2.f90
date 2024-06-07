program fptest
    !--------- -------- --------- --------- --------- --------- --------- --------- -----
    !
    ! Example program 2 for using the function parser module
    !
    !--------- -------- --------- --------- --------- --------- --------- --------- -----
    use FortranParser_parameters, only: rn
    use FortranParser, only: EquationParser
    implicit none
    type(EquationParser) :: eqParser
    integer, parameter :: nfunc = 3
    character(LEN=*), dimension(nfunc), parameter :: func = (/'vel*COS(beta)           ', &
                                                              'vel*SIN(beta)*COS(alpha)', &
                                                              'vel*SIN(beta)*SIN(alpha)'/)
    integer, parameter :: nvar = 3
    character(LEN=*), dimension(nvar), parameter :: var = (/'vel  ', &
                                                            'alpha', &
                                                            'beta '/)
    real(rn), dimension(nvar), parameter :: val = (/10., 1.5, 2.0/)
    real(rn)                                       :: res
    integer                                        :: i
    real(rn)                                       :: vel, alpha, beta
    !--------- -------- --------- --------- --------- --------- --------- --------- -----
    !
    write (*, *) '==> Bytecode evaluation:'
    do i = 1, nfunc
        eqParser = EquationParser(func(i), var)
        res = eqParser%evaluate(val)
        write (*, *) func(i), '=', res
    end do

    write (*, *) '==> Direct evaluation:'
    vel = val(1)
    alpha = val(2)
    beta = val(3)
    write (*, *) 'res=', vel * cos(beta)
    write (*, *) 'res=', vel * sin(beta) * cos(alpha)
    write (*, *) 'res=', vel * sin(beta) * sin(alpha)
    !
end program fptest
