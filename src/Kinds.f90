!> @ingroup group_all
!> @author davidpfister
module benchmark_kinds
    use, intrinsic :: iso_fortran_env, only: i1 => int8, &
                               i2 => int16, &
                               i4 => int32, &
                               i8 => int64, &
                               r4 => real32, &
                               r8 => real64, &
                               r16 => real128
    
    implicit none
    
    public :: i1, i2, i4, i8
    public :: r4, r8, r16
    
end module