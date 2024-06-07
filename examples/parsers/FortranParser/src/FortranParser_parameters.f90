!
! Copyright (c) 2000-2008, Roland Schmehl. All rights reserved.
!
! This software is distributable under the BSD license. See the terms of the
! BSD license in the documentation provided with this software.
!
module FortranParser_parameters
    !--------- -------- --------- --------- --------- --------- --------- --------- -----
    ! Specify data types
    !--------- -------- --------- --------- --------- --------- --------- --------- -----
    use, intrinsic :: iso_fortran_env, only: real64, int32
    implicit none

    integer, parameter :: rn = real64 ! Precision of real numbers
    integer, parameter :: is = int32 ! Data type of bytecode
end module FortranParser_parameters

