module parser_abstract
    use parameters
    
    implicit none
    
    type, abstract :: parser_x
        private
        character(:), allocatable, public :: name
    contains
        procedure(initialize_x), nopass, deferred :: initialize
        procedure(compute_x), nopass, deferred :: compute
    end type
    
    abstract interface
        subroutine initialize_x()
        end subroutine
        real(r8) function compute_x(eq)
            import r8
            character(*), intent(in) :: eq
        end function
    end interface
    
end module