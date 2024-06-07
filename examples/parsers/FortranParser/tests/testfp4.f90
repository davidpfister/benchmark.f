program fptest
    !--------- -------- --------- --------- --------- --------- --------- --------- -----
    !
    ! Example program 4 for using the function parser module
    !
    !--------- -------- --------- --------- --------- --------- --------- --------- -----
    use FortranParser_parameters, only: rn
    use FortranParser, only: EquationParser
    implicit none
    type(EquationParser) :: eqParser
    integer, parameter :: nfunc = 1
    character(LEN=*), dimension(nfunc), parameter :: func = (/'1.0e0 + 5.e1'/)
    integer, parameter :: nvar = 0
    character(LEN=*), dimension(nvar), parameter :: var = 'a'
    real(rn), dimension(nvar), parameter :: val = 0._rn
    real(rn)                                       :: res
    integer                                        :: i
    !--------- -------- --------- --------- --------- --------- --------- --------- -----
    !
    do i = 1, nfunc
        eqParser = EquationParser(func(i), var)
        res = eqParser%evaluate(val)
        write (*, *) func(i), '=', res
    end do

    !
end program fptest
