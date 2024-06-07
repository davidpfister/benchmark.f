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
    integer, parameter :: nfunc = 1
    character(LEN=*), dimension(nfunc), parameter :: func = (/'-x'/)
    integer, parameter :: nvar = 1
    character(LEN=*), dimension(nvar), parameter :: var = (/'x'/)
    real(rn), dimension(nvar), parameter :: val = (/2./)
    real(rn)                                       :: res
    integer                                        :: i
    real(rn)                                       :: x

    write (*, *) '==> Bytecode evaluation:'

    do i = 1, nfunc
        eqParser = EquationParser(func(i), var)
        res = eqParser%evaluate(val)
        write (*, *) func(i), '=', res
    end do

    write (*, *) '==> Direct evaluation:'
    x = val(1)
    write (*, *) '-x=', -x
    !
end program fptest
