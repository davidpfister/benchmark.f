program fptest
    !--------- -------- --------- --------- --------- --------- --------- --------- -----
    !
    ! Example program 3 for using the function parser module:
    !
    ! Assessing how fast the interpreter is compared against a direct evaluation
    !
    !--------- -------- --------- --------- --------- --------- --------- --------- -----
    use FortranParser_parameters, only: rn
    use FortranParser, only: EquationParser
    implicit none
    type(EquationParser) :: eqParser
    integer, parameter :: neval = 10000000
    integer, parameter :: nfunc = 3
    character(LEN=*), dimension(nfunc), parameter :: func = (/'vel*COS(beta)           ', &
                                                              'vel*SIN(beta)*COS(alpha)', &
                                                              'vel*SIN(beta)*SIN(alpha)'/)
    integer, parameter :: nvar = 3
    character(LEN=*), dimension(nvar), parameter :: var = (/'vel  ', &
                                                            'alpha', &
                                                            'beta '/)
    real(rn), dimension(nvar, neval)      :: val
    real(rn)                                       :: res
    integer                                        :: i, n
    real                                           :: rt1, rt2, rt3
    !--------- -------- --------- --------- --------- --------- --------- --------- -----
    !
    call random_number(val)

    call cpu_time(rt1)
    do i = 1, nfunc
        eqParser = EquationParser(func(i), var)
        do n = 1, neval
            res = eqParser%evaluate(val(:, i))
        end do
    end do
    call cpu_time(rt2)

    do n = 1, neval
        res = val(1, i) * cos(val(3, i))
        res = val(1, i) * sin(val(3, i)) * cos(val(2, i))
        res = val(1, i) * sin(val(3, i)) * sin(val(2, i))
    end do
    call cpu_time(rt3)
    write (*, *) 'Function evaluation:'
    write (*, *) '- Bytecode interpreter cpu time = ', rt2 - rt1
    write (*, *) '- Machine code         cpu time = ', rt3 - rt2, ' = ', (rt3 - rt2) / (rt2 - rt1) * 100., '%'
    !
end program fptest
