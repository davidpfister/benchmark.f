module allocation_library
    use benchmark_kinds

    implicit none
    
    private
    
    public :: naive_reallocation, &
              array_preallocation, &
              buffer_reallocation, &
              array_constructor
    
    contains
    
    subroutine buffer_reallocation(bs, n)
        integer, intent(in) :: bs
        integer, intent(in) :: n
        !private
        integer, allocatable :: array(:)
        integer :: i, j
        
        j = 0
        do i = 0, n
            call add_to(array, i, j, bs, finished = (i == n))
        end do
        
    contains
    
        pure subroutine add_to(vec, val, n, chunk_size, finished)
            integer, dimension(:), allocatable, intent(inout) :: vec
            integer, intent(in) :: val
            integer, intent(inout) :: n
            integer, intent(in) :: chunk_size
            logical, intent(in) :: finished
            !private
            integer, allocatable :: tmp(:)

            if (allocated(vec)) then
                if (n == size(vec)) then
                    ! add another chunk:
                    allocate (tmp(size(vec) + chunk_size))
                    tmp(1:size(vec)) = vec
                    call move_alloc(tmp, vec)
                end if
                n = n + 1
            else
                ! the first element:
                allocate (vec(chunk_size))
                n = 1
            end if

            vec(n) = val

            if (finished) then
                ! set vec to actual size (n):
                if (allocated(tmp)) deallocate (tmp)
                allocate (tmp(n))
                tmp = vec(1:n)
                call move_alloc(tmp, vec)
            end if

        end subroutine
            
    end subroutine
    
    subroutine naive_reallocation(n)
        integer, intent(in) :: n
        !private 
        integer, allocatable :: array(:)
        integer :: i
        
        allocate(array(0))
        do i = 1, n
            array = [array, i]
        end do
    end subroutine
    
    subroutine array_constructor(n)
        integer, intent(in) :: n
        !private 
        integer :: i
        integer :: array(n) 
        
        array = [(i, i = 1,n)]    
    end subroutine
    
    subroutine array_preallocation(n)
        integer, intent(in) :: n
        !private 
        integer, allocatable :: array(:)
        integer :: i
        
        allocate(array(n))
        do i = 1, n
            array(i) = i
        end do
    end subroutine
        
end module