module conversion
    use, intrinsic :: iso_c_binding
    use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_positive_inf, ieee_quiet_nan
    use, intrinsic :: iso_fortran_env, only: i1 => int8, i2 => int16, i4 => int32, i8 => int64, &
                                             r4 => real32, r8 => real64, r16 => real128
    implicit none
    private
    public :: cint, cint_p
    public :: cdbl, cdbl_p
    
    integer(kind=i2), parameter :: zero       = 0_i2
    integer(kind=i2), parameter :: one        = 1_i2
    integer(kind=i2), parameter :: nine       = 9_i2
    integer(kind=i2), parameter :: digit_0    = ichar('0',kind=i2)
    integer(kind=i2), parameter :: period     = ichar('.',kind=i2) - digit_0
    integer(kind=i2), parameter :: comma      = ichar(',',kind=i2) - digit_0
    integer(kind=i2), parameter :: minus_sign = ichar('-',kind=i2) - digit_0
    integer(kind=i2), parameter :: plus_sign  = ichar('+',kind=i2) - digit_0
    integer(kind=i2), parameter :: Inf        = ichar('I',kind=i2)
    integer(kind=i2), parameter :: NaN        = ichar('N',kind=i2)
    integer(kind=i2), parameter :: le         = ichar('e',kind=i2) - digit_0
    integer(kind=i2), parameter :: BE         = ichar('E',kind=i2) - digit_0
    integer(kind=i2), parameter :: ld         = ichar('d',kind=i2) - digit_0
    integer(kind=i2), parameter :: BD         = ichar('D',kind=i2) - digit_0
    integer(kind=i2), parameter :: LF = 10, CR = 13, WS = 32
    
    integer(kind=i2), parameter :: nwnb = 40 !> number of whole number factors
    integer(kind=i2), parameter :: nfnb = 40 !> number of fractional number factors
    real(r8), parameter :: whole_number_base(nwnb) =             &
        [1d39,  1d38,  1d37,  1d36,  1d35,  1d34, 1d33,  1d32,   & 
         1d31,  1d30,  1d29,  1d28,  1d27,  1d26, 1d25,  1d24,   &
         1d23,  1d22,  1d21,  1d20,  1d19,  1d18, 1d17,  1d16,   &
         1d15,  1d14,  1d13,  1d12,  1d11,  1d10,  1d9,   1d8,   &
         1d7,   1d6,   1d5,   1d4,   1d3,   1d2,   1d1,   1d0]
    real(r8), parameter :: fractional_base(nfnb)   =                 &
        [1d-1,  1d-2,  1d-3,  1d-4,  1d-5,  1d-6,  1d-7,  1d-8,  &
         1d-9,  1d-10, 1d-11, 1d-12, 1d-13, 1d-14, 1d-15, 1d-16, &
         1d-17, 1d-18, 1d-19, 1d-20, 1d-21, 1d-22, 1d-23, 1d-24, &
         1d-25, 1d-26, 1d-27, 1d-28, 1d-29, 1d-30, 1d-31, 1d-32, &
         1d-33, 1d-34, 1d-35, 1d-36, 1d-37, 1d-38, 1d-39, 1d-40 ]
    real(r8), parameter :: period_skip = 0d0
    real(r8), parameter :: expbase(nwnb+nfnb) = [whole_number_base, fractional_base]

    contains
    
    !---------------------------------------------
    ! String To Integer implementations
    !---------------------------------------------
    elemental function cint(s) result(int)
        ! -- In/out Variables
        character(*), intent(in) :: s !> input string
        integer :: int !> output integer value
        !private
        integer(1) :: p !> position within the number
        integer(1)  :: stat ! error status
        
        call cint32(s,int,p,stat)
    end function
    
    function cint_p(s,stat) result(int)
        character(:), pointer :: s !> input string
        integer :: int !> output integer value
        integer(1),intent(inout), optional :: stat
        !private
        integer(1) :: p !> position within the number
        integer(1) :: err
        
        call cint32(s,int,p,err)
        p = min( p , len(s) )
        s => s(p:)
        if(present(stat)) stat = err
    end function

    elemental subroutine cint32(s,int,p,stat)
        !> Return an unsigned 32-bit integer
        ! -- In/out Variables
        character(*), intent(in) :: s !> input string
        integer, intent(inout)  :: int !> output integer value
        integer(1), intent(out)  :: p !> position within the number
        integer(1), intent(out)  :: stat !> status upon succes of failure to read
        !private
        integer(1)  :: val 
        
        stat = 23 !> initialize error status with any number > 0
        
        ! Find first non white space
        p = mvs2nwsp(s)
        
        int = 0
        do while( p<=len(s) )
            val = iachar(s(p:p))-digit_0
            if( val >= 0 .and. val <= 9) then
                int = int*10 + val ; p = p + 1
            else
                exit
            end if
        end do
        stat = 0
    end subroutine

    !---------------------------------------------
    ! String To Real function interfaces
    !---------------------------------------------
    
    elemental function cdbl(s) result(r)
        ! -- In/out Variables
        character(*), intent(in) :: s !> input string
        real(r8) :: r !> Output real value
        !private
        integer(1) :: p !> position within the number
        integer(1) :: stat ! error status
        
        call cdbl_dp(s,r,p,stat)
    end function
    
    function cdbl_p(s,stat) result(r)
        ! -- In/out Variables
        character(:), pointer :: s !> input string
        real(r8) :: r                  !> Output real value
        integer(1),intent(inout), optional :: stat
        !private
        integer(1) :: p !> position within the number
        integer(1) :: err
        
        call cdbl_dp(s,r,p,err)
        p = min( p , len(s) )
        s => s(p:)
        if(present(stat)) stat = err
    end function
    
    !---------------------------------------------
    ! String To Real implementations
    !---------------------------------------------
    
    elemental subroutine cdbl_dp(s,r,p,stat)
        !> Sequentially unroll the character and get the sub integers composing the whole number, fraction and exponent
        ! -- In/out Variables
        character(*), intent(in) :: s !> input string
        real(r8), intent(inout)  :: r !> Output real value
        integer(1), intent(out)  :: p !> last position within the string
        integer(1), intent(out)  :: stat !> status upon success or failure to read
        !private
        integer(1)  :: sign, sige !> sign of integer number and exponential
        integer(r8) :: int_r8 !> long integer to capture fractional part
        integer     :: i_exp !> integer to capture whole number part
        integer(1)  :: i, pP, pE, val , resp
        
        stat = 23 !> initialize error status with any number > 0
        
        ! Find first non white space
        p = mvs2nwsp(s)
        
        ! Verify leading negative
        sign = 1
        if( iachar(s(p:p)) == minus_sign+digit_0 ) then
            sign = -1 ; p = p + 1
        end if
        if( iachar(s(p:p)) == Inf ) then
            r = sign*ieee_value(r,  ieee_positive_inf); return
        else if( iachar(s(p:p)) == NaN ) then
            r = ieee_value(r,  ieee_quiet_nan); return
        end if
        
        ! read whole and fractional number in a single integer
        pP = 127
        int_r8 = 0
        do i = p, min(19+p-1,len(s))
            val = iachar(s(i:i))-digit_0
            if( val >= 0 .and. val <= 9 ) then
                int_r8 = int_r8*10 + val
            else if( val == period ) then
                pP = i
            else
                exit
            end if
        end do
        pE = i ! Fix the exponent position
        do while( i<=len(s) )
           val = iachar(s(i:i))-digit_0
           if( val < 0 .or. val > 9 ) exit
           i = i + 1
        end do
        p = i
        resp = pE-min(pP,p) ! If no decimal indicator found it is taken as being in the current p position
        if( resp <= 0 ) resp = resp+1 
        
        ! Get exponential
        sige = 1
        if( p<len(s) ) then
            if( any([le,BE,ld,BD]+digit_0==iachar(s(p:p))) ) p = p + 1
            if( iachar(s(p:p)) == minus_sign+digit_0 ) then
                sige = -1
                p = p + 1
            else if( iachar(s(p:p)) == plus_sign+digit_0 ) then
                p = p + 1
            end if
        end if
        
        i_exp = 0
        do while( p<=len(s) )
            val = iachar(s(p:p))-digit_0
            if( val >= 0 .and. val <= 9) then
                i_exp = i_exp*10_i2 + val ; p = p + 1
            else
                exit
            end if
        end do

        r = sign*int_r8*expbase(nwnb-1+resp-sige*max(0,i_exp))
        stat = 0
    end subroutine

    !---------------------------------------------
    ! Utility functions
    !---------------------------------------------
    
    elemental function mvs2nwsp(s) result(p)
        !> get position of the next non white space character
        character(*),intent(in) :: s !> character chain
        integer(1) :: p !> position
        
        p = 1
        do while( p<len(s) .and. (iachar(s(p:p))==WS.or.iachar(s(p:p))==LF.or.iachar(s(p:p))==CR) ) 
            p = p + 1
        end do
    end function
    
    elemental function mvs2wsp(s) result(p)
        !> get position of the next white space character
        character(*),intent(in) :: s !> character chain
        integer(1) :: p !> position
        
        p = 1
        do while( p<len(s) .and. .not.(iachar(s(p:p))==WS.or.iachar(s(p:p))==LF.or.iachar(s(p:p))==CR) ) 
            p = p + 1
        end do
    end function
end module