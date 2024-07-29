module evaluate_precision
    implicit none; private

! Real kinds

    integer, parameter, public :: kr4 = selected_real_kind(6, 37) ! single precision real
    integer, parameter, public :: kr8 = selected_real_kind(15, 307) ! double precision real

! Integer kinds

    integer, parameter, public :: ki4 = selected_int_kind(9) ! single precision integer
    integer, parameter, public :: ki8 = selected_int_kind(18) ! double precision integer

!Complex kinds

    integer, parameter, public :: kc4 = kr4 ! single precision complex
    integer, parameter, public :: kc8 = kr8 ! double precision complex

end module
