module parser_factory
    use parser_abstract
    use runner1
    use runner2
    use runner3
    use runner4
    use runner5
    use runner6
    use runner7
    use runner8
    use runner9
#ifdef _INCLUDE_SYMENGINE
    use runner10
#endif
    use references
    
    
    implicit none
    
    private
    
    public :: eq_parsers, &
              parser_x
    
    character(80) :: eq_parsers(1:11)
    integer :: j
    
    data(eq_parsers(j), j=1, 11)/ &
        'equationparser', & !https://github.com/ScottBoyce/bif/blob/main/src/math_numbers/EquationParser.f90
        'evaluate', & !https://gbenthien.net/strings/index.html
        'feq-parse', & !https://github.com/fluidnumerics/feq-parse   
        'function_parser', & !https://github.com/jacobwilliams/fortran_function_parser
        'interpreter', & !https://github.com/sdm900/fortran_parser
        'fortranparser', & !https://github.com/jacopo-chevallard/FortranParser
        'fparser', & !http://fparser.sourceforge.net
        'fee', & !https://github.com/ivomarb/Fortran-Expression-Evaluator
        'M_calculator', & !https://urbanjost.github.io/M_calculator  
        'symengine', & !https://github.com/symengine/symengine.f90
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
        elseif (parser == 'evaluate') then
            allocate (e, source=eqnevaluate())
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
#ifdef _INCLUDE_SYMENGINE
            allocate (e, source=symengine_parser())
#endif
        elseif (parser == 'equationparser') then
            allocate (e, source=equation_parser())
        elseif (parser == 'reference') then
            allocate (e, source=reference())
        end if
    end function
end module