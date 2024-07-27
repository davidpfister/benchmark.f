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
        real(r8) function compute_x(i)
            import r8
            integer, intent(in) :: i 
        end function
    end interface
    
end module