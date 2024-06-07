program test_suite_M_calculator
use, intrinsic :: iso_fortran_env, only: &
& stdin => input_unit,   &
& stdout => output_unit, &
& stderr => error_unit
use, intrinsic :: iso_fortran_env, only : compiler_version, compiler_options
use M_calculator, only : calculator
! convenience routines
use M_calculator, only : inum0, rnum0, dnum0, snum0, expression
! constants
use M_calculator, only : iclen_calc, ixy_calc, icname_calc, x, y, values_len, values
!!use M_calculator, only : read_config
implicit none
logical, parameter :: T=.true., F=.false.
integer,parameter :: bug=0 ! gfortran-11 bug where function calls as arguments cause errors, but expressions do not

   !!EXAMPLE OF ALMOST JUST PRINTING VALUES!!if(almost(z,10*sin(3.1416d0/4.0d0),35,verbose=.true.))continue
   print '(4(a/))', &
      'This file was compiled by ', &
      compiler_version(),           &
      'using the options ',         &
      compiler_options()
      
call test_calculator()

call test_dnum0()
call test_inum0()
call test_expression()
call test_rnum0()
call test_snum0()

call test_c()
call test_juown1()

call test_funcs()

contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_calculator()
! iclen_calc : max length of expression or variable value as a string
integer, parameter         :: dp = kind(0.0d0)
character(len=iclen_calc) :: line
character(len=iclen_calc) :: outlin
character(len=iclen_calc) :: event
real(kind=dp)             :: rvalue
integer                   :: ierr

   ierr = 0
   call calculator('ownmode(1)', outlin, event, rvalue, ierr)

   ! activate user-defined function interface
   !!call unit_test('calculator', 0.eq.0, 'checking',100)

   line='1.3/sind(90)+1-20*2/3'
   call calculator(line, outlin, event, rvalue, ierr)
   select case (ierr)
      ! several different meanings to the error flag returned by calculator
   case (0)
      ! a numeric value was returned without error
      write (*, '(a,a,a)') trim(outlin), ' = ', trim(line)
   case (2)
      ! a string value was returned without error
      write (*, '(a)') trim(event(:int(rvalue)))
   case (1)
      ! a request for a message has been returned (from DUMP or FUNC)
      write (*, '(a,a)') 'message===>', trim(event(:len_trim(event)))
   case (-1)
      ! an error has occurred
      write (*, '(a,a)') 'error===>', trim(event(:len_trim(event)))
   case default
      ! this should not occur
      WRITE (6, '(A,i10)') '*CALCULATOR* UNEXPECTED IERR VALUE ', IERR
   end select

end subroutine test_calculator
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_c()
   !!call unit_test('c', 0.eq.0, 'checking',100)
end subroutine test_c
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_juown1()
   !!call unit_test('juown1', 0.eq.0, 'checking',100)
end subroutine test_juown1
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dnum0()
doubleprecision :: y, z
   Y=DNUM0('CI = 10 * sin(3.1416/4)')
   Z=DNUM0('CI')
  print*, y.eq.z .and. y == 10*sin(3.1416d0/4d0)

end subroutine test_dnum0
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_inum0()
character(len=:),allocatable :: string

   string='10/3'
   print*, inum0(string).eq.3

   string='(444/111+1)*10-5.0'
   print*, inum0(string).eq.45

   string='-10'
   print*, inum0(string).eq.-10

   string='+10'
   print*, inum0(string).eq.+10

end subroutine test_inum0
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_funcs()
character(len=iclen_calc) ::  outlin0
doubleprecision :: outval
integer :: ierr, ilen
character(len=:),allocatable :: string
!   call expression('A=3.4**5    ',outval,outlin0,ierr,ilen)
!   write(*,*)'value of expression is ',outval
!   write(*,*)'string representation of value is ',trim(outlin0)
!   write(*,*)'error flag value is ',ierr
!   write(*,*)'length of expression is ',ilen

   string='+10'

   call expression('xstore(1,10,20,30)',outval,outlin0,ierr,ilen)
   print*, inum0('x(2)').eq.20

    print*, inum0('  max(-100,0,20,40)   ').eq.40
   print*, inum0('  min(-100,0,20,40)   ').eq.-100
   print*, inum0('  sqrt(25)            ').eq.5
   print*, inum0('  log10(1000)         ').eq.3
   print*, inum0('  hypot(3,4)          ').eq.5

end subroutine test_funcs
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_expression()
character(len=iclen_calc) ::  outlin0
doubleprecision :: outval
character(len=:),allocatable :: string
integer :: ierr, ilen

   string='A=3.4**5    '
   call expression(string,outval,outlin0,ierr,ilen)
   print*, ierr==0
   print*, ilen==8

   string='$STR("The value is ",nint(40/4))'
   call expression(string,outval,outlin0,ierr,ilen)
   print*, ierr==2

   string='# this is a comment'
   !call unit_test('expression', ierr==1, 'expression is a comment',ierr)

end subroutine test_expression
!
!   string='A=sin(3.1416/5)'
!   !  -1 if an error occurred
!   !  0 if a numeric value is returned (value is in OUTVAL, string representation of the value is in OUTLIN2).
!   !  1 if no value was returned but a message was displayed (If a 'dump' or 'funcs' command was passed to the calculator).
!   !  2 if the expression evaluated to a string value instead of a numeric value (value is in OUTLIN0).
!   write(*,*)'value of expression is ',outval
!   write(*,*)'string representation of value is ',trim(outlin0)
!   
!   ! value of expression is    454.35424000000000
!   ! string representation of value is 454.35424
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_rnum0()
character(len=:),allocatable :: string

   string='10/2'
   print*, rnum0(string).eq.5
end subroutine test_rnum0
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_snum0()
character(len=:),allocatable :: string
   string='$str(10/2)'
   print*, snum0(string).eq.'5'
end subroutine test_snum0
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
end program test_suite_M_calculator
