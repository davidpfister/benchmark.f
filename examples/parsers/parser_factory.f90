module parser_factory
    use parser_abstract
    use test_M_calculator
    use test_FEQParse
    use test_function_parser
    use test_interpreter
    use test_fortranparser
    use test_fparser
    use test_fee
    use test_symengine
    use test_reference
    use test_equationparser
    
    implicit none
    
    private
    
    public :: parsers, &
              parser_x
    
    character(80) :: parsers(1:10)
    integer :: j
    
    data(parsers(j), j=1, 10)/ &
		'M_calculator', & !https://urbanjost.github.io/M_calculator
        'feq-parse', & !https://github.com/fluidnumerics/feq-parse
        'function_parser', & !https://github.com/jacobwilliams/fortran_function_parser
        'interpreter', & !https://github.com/sdm900/fortran_parser
        'fortranparser', & !https://github.com/jacopo-chevallard/FortranParser
        'fparser', & !http://fparser.sourceforge.net
        'fee', & !https://github.com/ivomarb/Fortran-Expression-Evaluator
        'symengine', & !https://github.com/symengine/symengine.f90
        'equationparser', & !https://github.com/ScottBoyce/bif/blob/main/src/math_numbers/EquationParser.f90
        'reference'/
    
    type, public :: factory
        private
        character(:), allocatable, public :: name
    contains
        procedure, public :: build
        procedure :: final
    end type
    
    interface factory
        module procedure :: constructor_null
    end interface

contains

    type(factory) function constructor_null() result(this)
        this%name = 'factory'
    end function
    
    subroutine final(this)
        class(factory), intent(inout) :: this
    end subroutine final

    function build(this, string) result(e)
        character(*), intent(in) :: string
        class(factory) :: this
        class(parser_x), allocatable :: e
        !private
        character(:), allocatable :: parser
        
        parser = trim(string)

        if (parser == 'M_calculator') then
            allocate (e, source=M_calculator_parser())
        elseif (parser == 'feq-parse') then
            allocate (e, source=FEQParse_parser())
        elseif (parser == 'function_parser') then
            allocate (e, source=ffunction_parser())
        elseif (parser == 'interpreter') then
            allocate (e, source=interpreter_parser())
        elseif (parser == 'fortranparser') then
            allocate (e, source=ffortranparser())
        elseif (parser == 'fparser') then
            allocate (e, source=ffparser())
        elseif (parser == 'fee') then
            allocate (e, source=fee_parser())
        elseif (parser == 'symengine') then
            allocate (e, source=symengine_parser())
        elseif (parser == 'equationparser') then
            allocate (e, source=equation_parser())
        elseif (parser == 'reference') then
            allocate (e, source=reference())
        end if
    end function
end module