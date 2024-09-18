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
#ifndef __INTEL_COMPILER    
        class(*), intent(in) :: bs
        class(*), intent(in) :: n
#else
        integer(i4), intent(in) :: bs
        integer(i4), intent(in) :: n
#endif
        !private
        integer, allocatable :: array(:)
        integer :: i, j
#ifndef __INTEL_COMPILER  
        select type(n)
        type is (integer(i4))
        select type(bs)
        type is (integer(i4))
#endif         
        j = 0
        do i = 0, n
            call add_to(array, i, j, bs, finished = (i == n))
        end do
#ifndef __INTEL_COMPILER 
        end select
        end select
#endif         
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
#ifndef __INTEL_COMPILER    
        class(*), intent(in) :: n
#else
        integer(i4), intent(in) :: n
#endif
        !private 
        integer, allocatable :: array(:)
        integer :: i
#ifndef __INTEL_COMPILER  
        select type(n)
        type is (integer(i4))
#endif           
        allocate(array(0))
        do i = 1, n
            array = [array, i]
        end do
#ifndef __INTEL_COMPILER 
    end select
#endif 
    end subroutine
    
    subroutine array_constructor(n)
#ifndef __INTEL_COMPILER    
        class(*), intent(in) :: n
#else
        integer(i4), intent(in) :: n
#endif
        !private 
        integer :: i
        integer, allocatable :: array(:) 
#ifndef __INTEL_COMPILER  
        select type(n)
        type is (integer(i4))
#endif       
        array = [(i, i = 1,n)]   
#ifndef __INTEL_COMPILER 
        end select
#endif 
    end subroutine
    
    subroutine array_preallocation(n)
#ifndef __INTEL_COMPILER    
        class(*), intent(in) :: n
#else
        integer(i4), intent(in) :: n
#endif
        !private 
        integer, allocatable :: array(:)
        integer :: i
#ifndef __INTEL_COMPILER  
        select type(n)
        type is (integer(i4))
#endif           
        allocate(array(n))
        do i = 1, n
            array(i) = i
        end do
#ifndef __INTEL_COMPILER 
        end select
#endif 
    end subroutine
        
end module