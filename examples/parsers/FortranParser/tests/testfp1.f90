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
    integer, parameter :: nfunc = 3
    character(LEN=*), dimension(nfunc), parameter :: func = (/'a0*b0 ', &
                                                              'a1/b1 ', &
                                                              'a3**b3'/)
    integer, parameter :: nvar = 6
    character(LEN=*), dimension(nvar), parameter :: var = (/'a0', &
                                                            'b0', &
                                                            'a1', &
                                                            'b1', &
                                                            'a3', &
                                                            'b3'/)
    real(rn), dimension(nvar), parameter :: val = (/1., 2., 3., 0., 5., 6./)
    real(rn)                                       :: res
    integer                                        :: i
    real(rn)                                       :: a0, b0, a1, b1, a3, b3
    !--------- -------- --------- --------- --------- --------- --------- --------- -----
    !
    write (*, *) '==> Bytecode evaluation:'
    do i = 1, nfunc
        eqParser = EquationParser(func(i), var)
        res = eqParser%evaluate(val)
        write (*, *) func(i), '=', res
    end do

    write (*, *) '==> Direct evaluation:'
    a0 = val(1)
    b0 = val(2)
    a1 = val(3)
    b1 = val(4)
    a3 = val(5)
    b3 = val(6)
    write (*, *) 'res=', a0 * b0
    write (*, *) 'res=', a1 / b1
    write (*, *) 'res=', a3**b3
    !
end program fptest
