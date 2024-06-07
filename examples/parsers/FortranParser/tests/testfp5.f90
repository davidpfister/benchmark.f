program fptest
    !--------- -------- --------- --------- --------- --------- --------- --------- -----
    !
    ! Example program 1 for using the function parser module
    !
    !--------- -------- --------- --------- --------- --------- --------- --------- -----
    use FortranParser_parameters, only: rn
    use FortranParser, only: EquationParser
    implicit none
    type(EquationParser) :: eqParser
    integer, parameter :: nfunc = 4
    character(LEN=*), dimension(nfunc), parameter :: func = (/'-1.0*x        ', &
                                                              '-x            ', &
                                                              'A*COS(B*x)+5  ', &
                                                              'A*COS(B*x)+5.0'/)
    integer, parameter :: nvar = 3
    character(LEN=*), dimension(nvar), parameter :: var = (/'x', &
                                                            'A', &
                                                            'B'/)
    real(rn), dimension(nvar), parameter :: val = (/2., 3., 4./)
    real(rn)                                       :: res
    integer                                        :: i
    real(rn)                                       :: x, A, B
    !--------- -------- --------- --------- --------- --------- --------- --------- -----
    !
    write (*, *) '==> Bytecode evaluation:'
    do i = 1, nfunc
        eqParser = EquationParser(func(i), var)
        res = eqParser%evaluate(val)
        write (*, *) func(i), '=', res
    end do

    write (*, *) '==> Direct evaluation:'
    x = val(1)
    A = val(2)
    B = val(3)
    write (*, *) '-1.0*x        =', -1.0 * x
    write (*, *) '-x            =', -x
    write (*, *) 'A*COS(B*x)+5  =', A * cos(B * x) + 5
    write (*, *) 'A*COS(B*X)+5.0=', A * cos(B * X) + 5.0
    !
end program fptest
