!>
!!##NAME
!!   calculator - [M_calculator] parse calculator expression and return numeric and string values
!!   (LICENSE:PD)
!!##SYNOPSIS
!!
!!   subroutine calculator(inline,outlin,mssg,slast,ierr)
!!
!!    character(len=*),intent=(in)           :: inline
!!    character(len=iclen_calc),intent=(out) :: outlin
!!    character(len=iclen_calc),intent=(out) :: mssg
!!    doubleprecision, intent=(out)          :: slast
!!    integer, intent=(out)                  :: ierr
!!
!!##DESCRIPTION
!!    CALCULATOR(3f) evaluates FORTRAN-like expressions. It can be used to add
!!    calculator-like abilities to your program.
!!
!!##OPTIONS
!!     inline  INLINE is a string expression up to (iclen_calc=512) characters long.
!!             The syntax of an expression is described in
!!             the main document of the Calculator Library.
!!     outlin  Returned numeric value as a string when IERR=0.
!!     mssg    MSSG is a string that can serve several purposes
!!             o Returned string value when IERR=2
!!             o Error message string when IERR=-1
!!             o Message from 'funcs' or 'dump' command when IERR=1
!!     slast   SLAST has different meanings depending on whether a string or number
!!             is being returned
!!             o REAL value set to last successfully calculated value when IERR=0
!!             o Number of characters in returned string variable when IERR=2
!!     ierr    status flag.
!!               -1  An error occurred
!!                0  A numeric value was returned
!!                1  A message was returned
!!                2  A string value was returned
!!##EXAMPLES
!!
!!   Example calculator program
!!
!!       program demo_calculator
!!       !compute(1f): line mode calculator program (that calls calculator(3f))
!!       use M_calculator, only: calculator,iclen_calc
!!       ! iclen_calc : max length of expression or variable value as a string
!!       implicit none
!!       integer,parameter         :: dp=kind(0.0d0)
!!       character(len=iclen_calc) :: line
!!       character(len=iclen_calc) :: outlin
!!       character(len=iclen_calc) :: event
!!       real(kind=dp)             :: rvalue
!!       integer                   :: ierr
!!       ierr=0
!!       call calculator('ownmode(1)',outlin,event,rvalue,ierr)
!!       ! activate user-defined function interface
!!       INFINITE: do
!!          read(*,'(a)',end=999)line
!!          if(line.eq.'.')stop
!!          call calculator(line,outlin,event,rvalue,ierr)
!!          select case (ierr)
!!          ! several different meanings to the error flag returned by calculator
!!          case(0)
!!          ! a numeric value was returned without error
!!            write(*,'(a,a,a)')trim(outlin),' = ',trim(line)
!!          case(2)
!!          ! a string value was returned without error
!!            write(*,'(a)')trim(event(:int(rvalue)))
!!          case(1)
!!          ! a request for a message has been returned (from DUMP or FUNC)
!!            write(*,'(a,a)')'message===>',trim(event(:len_trim(event)))
!!          case(-1)
!!          ! an error has occurred
!!            write(*,'(a,a)')'error===>',trim(event(:len_trim(event)))
!!          case default
!!          ! this should not occur
!!            WRITE(6,'(A,i10)')'*CALCULATOR* UNEXPECTED IERR VALUE ',IERR
!!          end select
!!       enddo INFINITE
!!       999 continue
!!       end program demo_calculator
!!
!!##SEE ALSO
!!     see INUM0(),RNUM0(),SNUM0(),,EXPRESSION().
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!>
!! AUTHOR   John S. Urban
!!##VERSION  1.0 19971123,20161218
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
module M_calculator

use, intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT,stdout=>OUTPUT_UNIT    ! access computing environment
!!implicit doubleprecision (a-h,o-z)
implicit none
private

integer, parameter                     :: db = kind(0.0d0) ! SELECTED_REAL_KIND(15,300) ! real*8
integer,parameter                      :: dp = kind(0.0d0)

integer,parameter,public               :: iclen_calc=512           ! max length of expression or variable value as a string
integer,parameter,public               :: ixy_calc=55555           ! number of variables in X() and Y() array
real(kind=dp),save,public              :: x(ixy_calc)=0.0_dp       ! x array for procedure funcs_
real(kind=dp),save,public              :: y(ixy_calc)=0.0_dp       ! y array for procedure funcs_

integer,parameter,public                  :: icname_calc=20        ! max length of a variable name

character(len=:),allocatable,save         :: keys_q(:)             ! contains the names of string variables
character(len=:),allocatable,save,public  :: values(:)             ! string variable values
integer,save,public,allocatable           :: values_len(:)         ! lengths of the string variable values

character(len=:),allocatable,save         :: keyr_q(:)             ! contains the names of numeric variables
real(kind=dp),save,allocatable            :: values_d(:)           ! numeric variable values
character(len=*),parameter                :: g='(*(g0,1x))'

public :: calculator
public :: stuff
public :: stuffa
! CONVENIENCE ROUTINES
public :: inum0        ! resolve a calculator string into a whole integer number
public :: rnum0        ! resolve a calculator string into a real number (return 0 on errors)
public :: dnum0        ! resolve a calculator string into a doubleprecision number (return 0 on error s)
public :: snum0        ! resolve a calculator expression into a string(return blank on errors)
public :: expression   ! call calculator() calculator and display messages

public :: set_mysub
public :: set_myfunc

integer,parameter                      :: ixyc_calc=50                   ! number of variables in $X() and $(Y) array
integer,parameter                      :: icbuf_calc=23*(iclen_calc/2+1) ! buffer for string as it is expanded


!  no check on whether line expansion ever causes line length to
!  exceed allowable number of characters.
!  number of characters to prevent over-expansion would currently be
!  23 digits per number max*(input number of characters/2+1).

character(len=iclen_calc)       :: mssge   ! for error message/messages /returning string value

character(len=iclen_calc),save  :: xc(ixyc_calc)=' '        ! $x array for procedure funcs_
character(len=iclen_calc),save  :: yc(ixyc_calc)=' '        ! $y array for procedure funcs_
character(len=iclen_calc),save  :: nc(ixyc_calc)=' '        ! $n array for procedure funcs_


character(len=iclen_calc),save  :: last='0.0'               ! string containing last answer (i.e. current value)
logical,save                    :: ownon=.false.            ! flag for whether to look for substitute_subroutine(3f)

integer,save                    :: ktoken                   ! count of number of token strings assembled
!
! requires
!  rand,
!
private :: a_to_d_                       ! returns a real value rval from a numeric character string chars.
private :: squeeze_
private :: stufftok_
private :: funcs_
private :: pows_
private :: given_name_get_stringvalue_
private :: parens_
private :: args_
private :: factors_
private :: expressions_
private :: help_funcs_

private :: juown1_placeholder
private :: c_placeholder

abstract interface
   subroutine juown1_interface(func,iflen,args,iargstp,n,fval,ctmp,ier)
      import dp
      character(len=*),intent(in)  :: func
      integer,intent(in)           :: iflen
      real(kind=dp),intent(in)     :: args(100)
      integer,intent(in)           :: iargstp(100)
      integer,intent(in)           :: n
      real(kind=dp)                :: fval
      character(len=*)             :: ctmp
      integer                      :: ier
   end subroutine juown1_interface
end interface

abstract interface
   real function c_interface(args,n)
      import dp
      integer,intent(in)         :: n
      real(kind=dp),intent(in)   :: args(n)
   end function c_interface
end interface
public c_interface
public juown1_interface

procedure(juown1_interface),pointer :: mysub => juown1_placeholder
procedure(c_interface),pointer      :: myfunc => c_placeholder

public locate        ! [M_list] find PLACE in sorted character array where value can be found or should be placed
   private locate_c
   private locate_d
public insert        ! [M_list] insert entry into a sorted allocatable array at specified position
   private insert_c
   private insert_d
   private insert_i
public replace       ! [M_list] replace entry by index from a sorted allocatable array if it is present
   private replace_c
   private replace_d
   private replace_i
public remove        ! [M_list] delete entry by index from a sorted allocatable array if it is present
   private remove_c
   private remove_d
   private remove_i

!character(len=*),parameter::ident_1="&
!&@(#)M_list::locate(3f): Generic subroutine locates where element is or should be in sorted allocatable array"
interface locate
   module procedure locate_c, locate_d
end interface

!character(len=*),parameter::ident_2="&
!&@(#)M_list::insert(3f): Generic subroutine inserts element into allocatable array at specified position"
interface insert
   module procedure insert_c, insert_d, insert_i
end interface

!character(len=*),parameter::ident_3="&
!&@(#)M_list::replace(3f): Generic subroutine replaces element from allocatable array at specified position"
interface replace
   module procedure replace_c, replace_d, replace_i 
end interface

!character(len=*),parameter::ident_4="&
!&@(#)M_list::remove(3f): Generic subroutine deletes element from allocatable array at specified position"
interface remove
   module procedure remove_c, remove_d, remove_i 
end interface
contains
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine set_myfunc(proc)
procedure(c_interface) :: proc
   myfunc => proc
end subroutine set_myfunc
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine set_mysub(proc)
procedure(juown1_interface) :: proc
   mysub => proc
end subroutine set_mysub
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
recursive subroutine calculator(inline,outlin,mssg,slast,ierr)
!
!     The goal is to create a procedure easily utilized from other
!     programs that takes a standard Fortran value statement and reduces
!     it down to a value, efficiently and using standard Fortran
!     standards where ever feasible.
!
!  Version 2.0: 03/13/87
!  Version 3.0: 07/11/2013
!  Version 5.0: 07/16/2013
!
!  o  adjacent powers are done left to right, not right to left
!  o  code does not prevent - and + beside an other operator.
!  o  no check on whether user input more characters than allowed.
!     no check on whether line expansion ever causes line length to
!     exceed allowable number of characters.
!     number of characters to prevent over-expansion would currently be
!     23 digits per number max*(input number of characters/2+1).
!  o  allowing for ixy_calc arguments in max and min seems too high. if reducing
!     array size helps significantly in costs, do so.
!  o  parentheses are required on a function call.
!  o  square brackets [] are equivalent to parenthesis ().
!===========================================================================--------------------------------------------------------
!  2. need a generic help function to list commands and functions
!  3. allow multiple expressions per line with a semi-colon between them
!     (like the parse functions).
!  4. make a function to fill x and y arrays, or to read values into them
!     from a file; and make some statistical functions that work on the
!     arrays.
!  6. allow user-written functions to be called from funcs_ routine.
!  7. allow for user-defined arrays and array operations.
!===========================================================================--------------------------------------------------------
!  12/07/87 --- put in an implicit real (a-h,o-z) statement in each
!              procedure so that it could quickly be changed to
!              implicit real*8 (a-h,o-z) for a vax. be careful of
!              type mismatch between external functions and the
!              real variables.
!              use following xedit commands where periods denote
!              spaces
!              c/implicit real../implicit real*8./ *
! 12/11/87  --- changed ifix calls to int calls as ifix on vax does
!              not allow real*8 in ifix calls
! 12/11/87  --- moving all prints out of column 1 so it is not picked
!              out by vax as carriage control.
! 12/28/87  --- put bn format specifier into a_to_d_ routine because
!              vax assumes zero fill
! 06/23/88  --- making a first cut at allowing string variables.
!               1. string variable names must start with a dollar-sign
!               2. strings can only be up to (iclen_calc) characters long
!               3. they will be returned in the message string to
!                  the calling program
!               4. input strings must be delimited with double quotes.
!                  to place a double quote into the string, put two
!                  double quotes adjacent to each other.
!               5. a flag value for ier to distinguish between string
!                  and numeric output?
!#----------------------------------------------------------------------------------------------------------------------------------
!subroutine calculator(inline,outlin,mssg,slast,ierr)

! ident_1="@(#) M_calculator calculator(3f) The procedure CALCULATOR(3f) acts like a calculator"

!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)            :: inline
character(len=iclen_calc),intent(out)  :: outlin
character(len=iclen_calc),intent(out)  :: mssg
real(kind=dp),intent(out)              :: slast
integer,intent(out)                    :: ierr
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=icbuf_calc)              :: line
character(len=iclen_calc)              :: varnam
character(len=iclen_calc)              :: junout
real(kind=dp),save                     :: rlast=0.0_dp
integer                                :: i10
integer                                :: i20
integer                                :: idum
integer                                :: imax
integer                                :: indx
integer                                :: iplace
integer                                :: istart
integer                                :: nchar2
integer                                :: nchard
!-----------------------------------------------------------------------------------------------------------------------------------
   line=inline                                      ! set working string to initial input line
   imax=len(inline)                                 ! determine the length of the input line
   mssg=' '                                         ! set returned message/error/string value string to a blank
   outlin=' '
   BIG: do                                          ! for $A=numeric and A=string
      ierr=1                                           ! set status flag to message mode
      mssge=' '                                        ! set message/error/string value in GLOBAL to a blank
      varnam=' '
      call squeeze_(line,imax,nchard,varnam,nchar2,ierr) ! preprocess the string: remove blanks and process special characters
                                                         ! also remove all quoted strings and replace them with a token
!-----------------------------------------------------------------------------------------------------------------------------------
      if(ierr.eq.-1)then                ! if an error occurred during preprocessing of the string, set returned message and quit
         slast=rlast                    ! set returned real value to last good calculated value
         mssg=mssge                     ! place internal message from GLOBAL into message returned to user
         return
      elseif(nchard.eq.0)then  ! if a blank input string was entered report it as an error and quit
         ierr=-1
         mssg='*calculator* input line was empty'
      elseif(line(1:nchard).eq.'dump')then ! process dump command
         write(*,g)line(1:nchard)
         write(*,g)' current value= ',last
         write(*,g)' variable name       variable value     '
         if(allocated(keyr_q))then
            do i10=1,size(keyr_q)
               if(keyr_q(i10).ne.' ')then
                  write(junout,'('' '',2a,g24.16e3)')keyr_q(i10),' ',values_d(i10)
                  write(*,g)trim(junout)
               endif
            enddo
         endif
         if(allocated(keys_q))then
            do i20=1,size(keys_q)
               if(keys_q(i20).ne.' ')then
                  write(junout,'('' '',3a)')keys_q(i20),' ',values(i20)(:values_len(i20))
                  write(*,g)trim(junout)
               endif
            enddo
         endif
         mssg='variable listing complete'
      elseif(line(1:nchard).eq.'funcs') then     ! process funcs command
         call help_funcs_()
         mssg='function listing complete'
!-----------------------------------------------------------------------------------------------------------------------------------
      else                                                ! this is an input line to process
         call parens_(line,nchard,ierr)                   ! process the command
         if(ierr.eq.0)then                 ! if no errors occurred set output string, store the value as last, store any variable
                                           ! numeric value with no errors, assume nchard is 23 or less
            outlin=line(1:nchard)                         ! set string output value
            last=line(1:nchard)                           ! store last value (for use with question-mark token)
            call a_to_d_(last(1:nchard),rlast,idum)       ! set real number output value
            if(nchar2.ne.0.and.varnam(1:1).ne.'$')then    ! if the statement defines a variable make sure variable name is stored
               call locate(keyr_q,varnam(:nchar2),indx,ierr) ! determine placement of the variable and whether it is new
               if(ierr.eq.-1)then
                  slast=rlast                             ! set returned real value to last good calculated value
                  mssg=mssge                              ! place internal message from GLOBAL into message returned to user
                  return
               endif
               if(indx.le.0)then                          ! if the variable needs added, add it
                  istart=iabs(indx)
                  call insert(keyr_q,varnam(:nchar2),istart)
                  call insert(values_d,0.0d0,istart)
               endif
               call a_to_d_(last(1:nchard),values_d(iabs(indx)),ierr)  ! store a defined variable's value
            elseif(nchar2.ne.0)then                       ! numeric value to string
               line(:)=' '
               line=varnam(:nchar2)//'="'//last(1:nchard)//'"'
               imax=len_trim(line)                        ! determine the length of the input line
               cycle BIG
            endif
         elseif(ierr.eq.2)then ! returned output is not numeric, but alphanumeric (it is a string)
!!!!!!!  could return string values directly instead of thru message field
!!!!!!!  make sure normal output values are not left indeterminate
            mssg=mssge                                    ! set returned string value to returned string value
            if(nchar2.ne.0.and.varnam(1:1).eq.'$')then    ! if the statement defines a variable make sure variable name is stored
               call locate(keys_q,varnam(:nchar2),indx,ierr) ! determine placement of the variable and whether it is new
               if(ierr.eq.-1)then
                  slast=rlast                             ! set returned real value to last good calculated value
                  mssg=mssge                              ! place internal message from GLOBAL into message returned to user
                  return
               endif
               iplace=iabs(indx)
               if(indx.le.0)then                             ! if the variable needs added, add it
                  call insert(keys_q,varnam(:nchar2),iplace) ! adding the new variable name to the variable name array
                  call insert(values,' '            ,iplace)
                  call insert(values_len,0              ,iplace)
               endif
               call replace(values,mssg,iplace)
               call replace(values_len,len_trim(mssg),iplace)
               rlast=dble(values_len(iplace))             ! returned value is length of string when string is returned
            elseif(nchar2.ne.0)then                       ! string but being stored to numeric variable
                line=varnam(:nchar2)//'='//mssg
                imax=len_trim(line)                       ! determine the length of the input line
                cycle BIG
            else                                          ! a string function with an assignment to it (for example "Hello"
               rlast=len_trim(mssg)                       ! probably should pass message length up from someplace
            endif
         endif
         mssg=mssge
      endif
      exit BIG
   enddo BIG
   slast=rlast                                            ! set returned value to last successfully calculated real value
end subroutine calculator
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##RETURNS
!!##EXAMPLE
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine help_funcs_()

! ident_2="@(#) M_calculator help_funcs_(3fp) prints help for calculator functions"

character(len=80),allocatable :: help_text(:)
integer                       :: i
help_text=[ &
&'--------------------------------------------------------------------------------',&
&'standard functions available:                                                   ',&
&'--------------------------------------------------------------------------------',&
!&'--------------------------------------------------------------------------------',&
!&' c(                   : user-defined function                                   ',&
!&' ownmode(             : call user-defined procedures                            ',&
&'--------------------------------------------------------------------------------',&
&' len_trim($value)         : number of characters trimming trailing spaces       ',&
&' index($value,$match)     : return position $match occurs in $value or zero     ',&
&' sign(val1,val2)          : magnitude of val1 with the sign of val2             ',&
&' real(value)              : conversion to real type                             ',&
&' matchw($string,$pattern) : wildcard match;*=any string, ?=any character        ',&
&' str($str|expr,....)      : append as strings and then convert to number        ',&
&' round(value,digits)      :                                                     ',&
&' ichar($value)            : return ASCII Decimal Equivalent of character        ',&
&' $char(value)             : return character given ASCII Decimal Equivalent     ',&
&' $f(format,value)         : using FORMAT to create it convert number to string  ',&
&' $repeat(string,count)    : repeat string count times                           ',&
&' $if(expr,$val1,$val2)    : if expr==0 return $val1, else return $val2          ',&
&' if(expr,val1,val2)       : if expr==0 return val1, else return val2            ',&
&' hypot(x,y)               : Euclidean distance function                         ',&
&'--------------------------------------------------------------------------------',&
&'WHOLE NUMBERS:                                                                  ',&
&' aint(value) : truncation toward zero to a whole number                         ',&
&' anint(value): nearest whole number                                             ',&
&' int(value)  : conversion to integer type                                       ',&
&' nint(value) : nearest integer                                                  ',&
&' floor(A)    : greatest integer less than or equal to A                         ',&
&' ceiling(A)  : least integer greater than or equal to A                         ',&
&'--------------------------------------------------------------------------------',&
&'MISCELLANEOUS:                                                                  ',&
&' max(v1,v2,v3,...v50)  : maximum value of list                                  ',&
&' min(v1,v2,v3,...v50)  : minimum value of list                                  ',&
&' dim(x,y)    : maximum of X-Y and zero                                          ',&
&' frac(A)     : fractional part of A (A - INT(A))                                ',&
&' mod(A,P)    : remainder function                                               ',&
&' abs(value)  : absolute value                                                   ',&
&' exp(value)  : exponent of value                                                ',&
&'BESSEL FUNCTIONS:                                                               ',&
&' bessel_j0 : of first kind of order 0  | bessel_j1 : of first kind of order 1   ',&
&' bessel_y0 : of second kind of order 0 | bessel_y1 : of second kind of order 1  ',&
&' bessel_jn : of first kind             | bessel_yn : of second kind             ',&
&'NUMERIC FUNCTIONS:                                                              ',&
&' sqrt(value) : return square root of value                                      ',&
&' log(v1)     : logarithm of value to base e                                     ',&
&' log10(v1)   : logarithm of value to base 10                                    ',&
&'--------------------------------------------------------------------------------',&
&'RANDOM NUMBERS:                                                                 ',&
&' srand(seed_value) : set seed value for rand()                                  ',&
&' rand()            : random number                                              ',&
&'--------------------------------------------------------------------------------',&
&'SYSTEM:                                                                         ',&
&' $setenv(name,value),$se(name,value)   : set environment variable value         ',&
&' $getenv(name),$ge(name)               : get environment variable value         ',&
&' $sh(command)                          : return output of system command        ',&
&'--------------------------------------------------------------------------------',&
&'ARRAY STORAGE:                                                                  ',&
&' $nstore(start_index,$value1,$value2,$value3,....) | $n(index)                  ',&
&' $xstore(start_index,$value1,$value2,$value3,....) | $x(index)                  ',&
&' $ystore(start_index,$value1,$value2,$value3,....) | $y(index)                  ',&
&' xstore(start_index,value1,value2,value3,....)     | x(index)                   ',&
&' ystore(start_index,value1,value2,value3,....)     | y(index)                   ',&
&'--------------------------------------------------------------------------------',&
&'STRING MODIFICATION:                                                            ',&
&' $l($input_string)        : convert string to lowercase                         ',&
&' $u($input_string)        : convert string to uppercase                         ',&
&' $substr($input_string,start_column,end_column)                                 ',&
&' $str($a|e,$a|e,$a|e,....):append string and value expressions into string      ',&
&'--------------------------------------------------------------------------------',&
&'CALENDAR:                                                                       ',&
&' ye(),year()     : current year       | mo(),month()    : current month         ',&
&' da(),day()      : current day        | ho(),hour()     : current hour          ',&
&' tz(),timezone() : current timezone   | mi(),minute()   : current minute        ',&
&' se(),second()   : current second     |                                         ',&
&' $mo([n])        : name of month      |                                         ',&
&'--------------------------------------------------------------------------------',&
&'TRIGONOMETRIC:                                                                  ',&
&' cos(radians) : cosine  | acos(x/r)   | cosh(x)  | acosh(x)  | cosd(degrees)    ',&
&' sin(radians) : sine    | asin(y/r)   | sinh(x)  | asinh(x)  | sind(degrees)    ',&
&' tan(radians) : tangent | atan(y/x)   | tanh(x)  | atanh(x)  | tand(degrees)    ',&
&'                        | atan2(x,y)  |                                         ',&
&'--------------------------------------------------------------------------------',&
&'UNIT CONVERSION:                                                                ',&
&' c2f(c) : centigrade to Fahrenheit    | f2c(f) : Fahrenheit to centigrade       ',&
&' d2r(d) : degrees to radians          | r2d(r) : radians to degrees             ',&
&'--------------------------------------------------------------------------------',&
&'LOGICAL:                                                                        ',&
&' ge(a,b) : greater than or equal to   | le(a,b) : A less than or equal to B     ',&
&' gt(a,b) : A greater than B           | lt(a,b) : A less than B                 ',&
&' eq(a,b) : A equal to B               | ne(a,b) : A not equal to B              ',&
&' in(lower_bound,test_value,upper_bound) : test if value in given range          ',&
&'LEXICAL LOGICAL:                                                                ',&
&' lge($a,$b): greater than or equal to | lle($a,$b): A less than or equal to B   ',&
&' lgt($a,$b): A greater than B         | llt($a,$b): A less than B               ',&
&' leq($a,$b): A equal to B             | lne($a,$b): A not equal to B            ',&
&'--------------------------------------------------------------------------------',&
&'HIGHER FUNCTIONS:                                                               ',&
&' fraction(x): fraction part of model | exponent(x) : largest exponent           ',&
&' gamma(x): gamma function            | log_gamma(x): logarithm of gamma function',&
&' tiny()  : smallest number           | huge():       largest number             ',&
!' erf(x), erfc_scaled(x): Error function | erfc(x): Complementary error function ',&
&'--------------------------------------------------------------------------------',&
&'                                                                                ']
   do i=1,size(help_text)
      write(*,g)help_text(i)
   enddo
end subroutine help_funcs_
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    parens_(3fp) - [M_calculator] crack out the parenthesis and solve
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    recursive subroutine parens_(string,nchar,ier)
!!    character(len=*)             :: string
!!    integer,intent(inout)        :: nchar
!!    integer,intent(out)          :: ier
!!##DESCRIPTION
!!    crack out the parenthesis and solve
!!##OPTIONS
!!    string  input string to expand and return
!!    nchar   length of string on input and output
!!    ier     status code
!!
!!              0=good numeric return
!!              2=good alphameric return
!!             -1=error occurred, message is in mssge
!!##RETURNS
!!##EXAMPLE
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
recursive subroutine parens_(string,nchar,ier)

! ident_3="@(#) M_calculator parens_(3fp) crack out the parenthesis and solve"

character(len=*)             :: string
integer,intent(inout)        :: nchar
integer,intent(out)          :: ier

character(len=icbuf_calc)    :: wstrng
character(len=:),allocatable :: dummy
integer                      :: imax
integer                      :: ileft
integer                      :: iright
integer                      :: i
integer                      :: iz
integer                      :: iwnchr
real(kind=dp)                :: rdum
!#----------------------------------------------------------------------------------------------------------------------------------
   imax=nchar
   ier=0
   INFINITE: do
!#----------------------------------------------------------------------------------------------------------------------------------
   ileft=0                                    ! where rightmost left paren was found
   do i=imax,1,-1                             ! find rightmost left paren
      if(string(i:i).eq.'(')then
         ileft=i
         exit
      endif
   enddo
!#----------------------------------------------------------------------------------------------------------------------------------
      if(ileft.eq.0)then                          ! no left parenthesis was found; finish up
         if(index(string(:nchar),')').ne.0) then  ! if here there are no left paren. check for an (unmatched) right paren
            ier=-1
            mssge='*parens_* extraneous right parenthesis found'
         else
   !        no parenthesis left, reduce possible expression to a single value primitive and quit
   !        a potential problem is that a blank string or () would end up here too.
            call expressions_(string,nchar,rdum,ier)
         endif
         return
      endif
!#----------------------------------------------------------------------------------------------------------------------------------
      iright=index(string(ileft:nchar),')') ! left parenthesis was found; find matching right paren
      if(iright.eq.0) then
         ier=-1
         mssge='*parens_* right parenthesis missing'
         return
      endif
!#----------------------------------------------------------------------------------------------------------------------------------
      iright=iright+ileft-1  !there was a matched set of paren remaining in the string
      iz=1  ! check now to see if this is a function call. search for an operator
!     if ileft is 1, then last set of parenthesis,(and for an expression)
      if(ileft.ne.1)then
         do i=ileft-1,1,-1
            iz=i
            if(index('#=*/(,',string(i:i)).ne.0)then
               iz=iz+1
               goto 11
            endif
         enddo
!        if here, a function call begins the string, as iz=1 but ileft doesn't
      endif
!=======================================================================------------------------------------------------------------
!     iz=position beginning current primitive's string
!     ileft=position of opening parenthesis for this primitive
!     iright=position of end and right parenthesis for this string
11    continue
      if(iz.eq.ileft)then  ! if ileft eq iz then a parenthesized expression, not a function call
         wstrng=string(ileft+1:iright-1)
         iwnchr=iright-1-(ileft+1)+1
         call expressions_(wstrng,iwnchr,rdum,ier)
      else
         wstrng=string(iz:iright)
         iwnchr=iright-iz+1
         call funcs_(wstrng,iwnchr,ier)
      endif
      if(ier.eq.-1)return !     if an error occurred in expressions_ or funcs_, then return
      ! restring the evaluated primitive back into the main string
      ! remember that if an expression, iz=ileft
      ! last set of -matched- parentheses, and entire string was evaluated
      if(iz.eq.1.and.iright.eq.nchar)then
         dummy=wstrng(:iwnchr)
         nchar=iwnchr
!        last set of -matched- parentheses, but other characters still to right
      elseif(iz.eq.1)then
         dummy=wstrng(:iwnchr)//string(iright+1:nchar)
         nchar=iwnchr+nchar-iright
      elseif(iright.eq.nchar)then
!        last expression evaluated was at end of string
         dummy=string(:iz-1)//wstrng(:iwnchr)
         nchar=iz-1+iwnchr
      else
!        last expression evaluated was in middle of string
         dummy=string(:iz-1)//wstrng(:iwnchr)//string(iright+1:nchar)
         nchar=iz-1+iwnchr+nchar-iright
      endif
!     set last place to look for a left parenthesis to one to the left
!     of the beginning of the primitive just reduced, or to a 1 so that
!     the loop looking for the left parenthesis doesn't look for a
!     parenthesis at position 0:0
      imax=max(iz-1,1)
      string=dummy
   enddo INFINITE
end subroutine parens_
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    funcs_(3fp) - [M_calculator]given string of form name(p1,p2,...) (p(i) are non-parenthesized expressions)
!!    call procedure "name" with those values as parameters.
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    recursive subroutine funcs_(wstrng,nchars,ier)
!!
!!     character(len=*)                    :: wstrng
!!##DESCRIPTION
!!##OPTIONS
!!##RETURNS
!!##EXAMPLE
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
recursive subroutine funcs_(wstrng,nchars,ier)

! ident_4="@(#) M_calculator funcs_(3fp) given string of form name(p1 p2 ...) (p(i) are non-parenthesized expressions) call procedure "name""

character(len=*)                    :: wstrng
integer                             :: nchars
integer                             :: ier

integer,parameter                   :: iargs=100
character(len=10),save              :: days(7)
character(len=10),save              :: months(12)
character(len=iclen_calc)           :: ctmp
character(len=iclen_calc)           :: ctmp2
character(len=iclen_calc)           :: junout
character(len=iclen_calc)           :: cnum
character(len=icname_calc)          :: wstrng2

real(kind=dp)                       :: args(iargs)

real(kind=dp)                       :: arg1
real(kind=dp)                       :: arg2
real(kind=dp)                       :: bottom
real(kind=dp)                       :: false
real(kind=dp)                       :: fval
real(kind=dp)                       :: top
real(kind=dp)                       :: true
real(kind=dp)                       :: val

real,external                       :: c

!!integer,save                        :: ikeepran=22
integer                             :: i
integer                             :: i1
integer                             :: i1010
integer                             :: i1033
integer                             :: i1060
integer                             :: i1066
integer                             :: i2
integer                             :: i2020
integer                             :: i3030
integer                             :: i410
integer                             :: i440
integer                             :: i520
integer                             :: iargs_type(iargs)
integer                             :: ibegin(ixyc_calc),iterm(ixyc_calc)
integer                             :: icalen
integer                             :: icount
integer                             :: idarray(8)
integer                             :: idig
integer                             :: idum
integer                             :: iend
integer                             :: iend1
integer                             :: iend2
integer                             :: iflen
integer                             :: ii
integer                             :: iii
integer                             :: ileft
integer                             :: ilen
integer                             :: in
integer                             :: indexout
integer                             :: ios
integer                             :: iright
integer                             :: istart
integer                             :: istore
integer                             :: istoreat
integer                             :: isub
integer                             :: itype
integer                             :: ival
integer                             :: ivalue
integer                             :: jend
integer                             :: n
integer                             :: ierr
integer                             :: ii2

intrinsic                           :: abs,aint,anint,exp,nint,int,log,log10
intrinsic                           :: acos,asin,atan,cos,cosh,sin,sinh,tan,tanh
intrinsic                           :: sqrt,atan2,dim,mod,sign,max,min
!-----------------------------------------------------------------------------------------------------------------------------------
   data months/'January','February','March','April','May','June','July','August','September','October','November','December'/
   data days/'Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'/

   TRUE=0.0d0
   FALSE=1.0d0
   ier=0
   iright=nchars-1
   ileft=index(wstrng(1:nchars),'(')+1
   iend=ileft-2
   iflen=iright-ileft+1
!  n=number of parameters found
   if(iright-ileft.lt.0)then ! if call such as fx() expression string is null
      n=0
   else ! take string of expressions separated by commas and place values into an array and return how many values were found
      call args_(wstrng(ileft:iright),iflen,args,iargs_type,n,ier,100)
      if(ier.eq.-1)then
         goto 999
      else
         ier=0 ! ier could be 2 from args_()
      endif
   endif
   wstrng2=' '
   wstrng2(:iend)=lower(wstrng(:iend))
   fval=0.0d0
   if(ier.eq.-1)then
      goto 999
   endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
select case (wstrng2(:iend))
case("abs","aint","anint","ceil","ceiling","floor","frac","int","nint",&
    &"d2r","r2d",&
    &"c2f","f2c",&
    &"gamma","log_gamma",&
    &"log","log10","exp",&
    &"bessel_j0","bessel_j1","bessel_y0","bessel_y1",&
    &"erf","erfc","erfc_scaled",&
    &"sin","cos","tan",&
    &"sind","cosd","tand",&
    &"sinh","cosh","tanh",&
    &"asin","acos","atan",&
    &"asinh","acosh","atanh",&
!   &"cpu_time",&
    &"exponent","fraction",&
    &"real","sqrt")
      if(n.ne.1)then                                                    ! check number of parameters
        mssge='*funcs_* incorrect number of parameters in '//wstrng2(:iend)
        ier=-1
      elseif(iargs_type(1).ne.0)then                                    ! check type of parameters
        mssge='*funcs_* parameter not numeric in '//wstrng2(:iend)
        ier=-1
      else                                                              ! single numeric argument
         select case (wstrng2(:iend))
!=======================================================================------------------------------------------------------------
         case("acos");

         if(args(1).gt.1.or.args(1).lt.-1)then
           mssge='*acos* parameter not in range -1 >= value <=1'
           ier=-1
         else
            fval= acos(args(1))
         endif
         case("atan");   fval= atan(args(1))
         case("asin");   fval= asin(args(1))

         !case("cpu_time");   fval= cpu_time(args(1))
         case("fraction");   fval= fraction(args(1))
         case("exponent");   fval= exponent(args(1))
         case("gamma");      fval= gamma(args(1))
         case("log_gamma");  fval= log_gamma(args(1))

         case("cos");    fval= cos(args(1))
         case("sin");    fval= sin(args(1))
         case("tan");    fval= tan(args(1))

         case("acosh");    fval= acosh(args(1))
         case("asinh");    fval= asinh(args(1))
         case("atanh");    fval= atanh(args(1))

         case("cosd");   fval= cos(args(1)*acos(-1.0d0)/180.d0)
         case("sind");   fval= sin(args(1)*acos(-1.0d0)/180.d0)
         case("tand");   fval= tan(args(1)*acos(-1.0d0)/180.d0)

         case("cosh");   fval= cosh(args(1))
         case("sinh");   fval= sinh(args(1))
         case("tanh");   fval= tanh(args(1))

         case("erf");         fval= erf(args(1))
         case("erfc");        fval= erfc(args(1))
         case("erfc_scaled"); fval= erfc_scaled(args(1))

         case("d2r");    fval= args(1)*acos(-1.0d0)/180.d0
         case("r2d");    fval= args(1)*180.d0/acos(-1.0d0)

         case("c2f");    fval= (args(1)+40.0d0)*9.0d0/5.0d0 - 40.0d0
         case("f2c");    fval= (args(1)+40.0d0)*5.0d0/9.0d0 - 40.0d0

         case("bessel_j0");    fval= bessel_j0(args(1))
         case("bessel_j1");    fval= bessel_j1(args(1))
         case("bessel_y0");    fval= bessel_y0(args(1))
         case("bessel_y1");    fval= bessel_y1(args(1))

         case("abs");    fval= abs(args(1))
         case("aint");   fval= aint(args(1))
         case("anint");  fval= anint(args(1))
         case("ceil","ceiling");   fval=ceiling(real(args(1)))
         case("exp");    fval= exp(args(1))
         case("floor");  fval= floor(real(args(1)))
         case("frac");   fval= args(1)-int(args(1))
         case("int");    fval= int(args(1))
         case("nint");   fval= nint(args(1))
         case("real");   fval= real(args(1))
         case("sqrt");   fval= sqrt(args(1))
!=======================================================================------------------------------------------------------------
         case("log")
            if(args(1).le.0.0d0)then                                          ! check for appropriate value range for function
               write(*,g)'*log* ERROR: cannot take log of ',real(args(1))
            else                                                              ! call function with one positive numeric parameter
               fval= log(args(1))
            endif
!=======================================================================------------------------------------------------------------
         case("log10")
            if(args(1).le.0.0d0)then                                          ! check for appropriate value range for function
               write(*,g)'*log10* ERROR: cannot take log of ',real(args(1))
            else                                                              ! call function with one positive numeric parameter
               fval= log10(args(1))
            endif
!=======================================================================------------------------------------------------------------
         end select
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("atan2","dim","mod","bessel_jn","bessel_yn","sign","hypot","modulo","scale")
      if(n.ne.2)then                                                           ! check number of parameters
        mssge='*funcs_* incorrect number of parameters in '//wstrng2(:iend)
        ier=-1
      elseif(.not.all(iargs_type(1:2).eq.0))then                                ! check type of parameters
        mssge='*funcs_* parameters not all numeric in '//wstrng2(:iend)
        ier=-1
      else                                                                     ! single numeric argument
         select case (wstrng2(:iend))
         case("atan2");      fval=  atan2       ( args(1),       args(2)       )
         case("dim");        fval=  dim         ( args(1),       args(2)       )
         case("mod");        fval=  mod         ( args(1),       args(2)       )
         case("modulo");     fval=  modulo      ( args(1),       args(2)       )
         case("scale");      fval=  scale       ( args(1),       int(args(2))  )
         case("bessel_jn");  fval=  bessel_jn   ( int(args(1)),  args(2)       )
         case("bessel_yn");  fval=  bessel_yn   ( int(args(1)),  args(2)       )
         case("btest");      fval=  merge(TRUE, FALSE,  btest( int(args(1)),  int(args(2))  ) )
         case("sign");       fval=  sign        ( args(1),       args(2)       )
         case("hypot");      fval=  hypot       ( args(1),       args(2)       )
         end select
      endif
!=======================================================================------------------------------------------------------------
case("tiny")
      fval=tiny(0.d0)
case("epsilon")
      fval=epsilon(0.d0)
case("huge")
      fval=huge(0.d0)
!=======================================================================------------------------------------------------------------
case("x");
      ivalue=int(args(1)+0.5d0)
      if(ivalue.lt.1.or.ivalue.gt.ixy_calc)then              ! if value not at least 1, or if not less than ixy_calc, report it
        mssge='*funcs_* illegal subscript value for x array'
        ier=-1
      else
        fval= x(ivalue)
      endif
!=======================================================================------------------------------------------------------------
case("y")
      ivalue=int(args(1)+0.5d0)
!     if value not at least 1, make it 1. if not less than ixy_calc, make it ixy_calc
      if(ivalue.lt.1.or.ivalue.gt.ixy_calc)then
         mssge='*funcs_* illegal subscript value for y array'
         ier=-1
      else
         fval= y(ivalue)
      endif
!=======================================================================------------------------------------------------------------
case("max")
      if(n.lt.1)then
         ier=-1
         mssge='*max* incorrect number of parameters for '//wstrng(:iend)
      elseif(.not.all(iargs_type(1:n).eq.0))then ! check type of parameters
         ier=-1
         mssge='*max* illegal parameter type (must be numeric)'
      else
         fval=args(1)
         do i=2,n
            fval=max(fval,args(i))
         enddo
      endif
!=======================================================================------------------------------------------------------------
case("min")
       if(n.lt.1)then
         ier=-1
         mssge='incorrect number of parameters for '//wstrng(:iend)
      elseif(.not.all(iargs_type(1:n).eq.0))then ! check type of parameters
         ier=-1
         mssge='*min* illegal parameter type (must be numeric)'
       else
          fval=args(1)
          do i=2,n
             fval=min(fval,args(i))
          enddo
       endif
!=======================================================================------------------------------------------------------------
case("xstore","ystore")                                        ! xstore function===>(where_to_start,value1,value2,value3...)
      if(n.lt.2)then                                           ! need at least subscript to start storing at and a value
         ier=-1
         mssge='incorrect number of parameters for '//wstrng(:iend)
         fval=0.0d0
      else                                                     ! at least two values so something can be stored
         istoreat=int(args(1)+0.50d0)                          ! array subscript to start storing values at
         if(istoreat.lt.1.or.istoreat+n-2.gt.ixy_calc)then     ! ignore -entire- function call if a bad subscript reference was made
            mssge='*funcs_* illegal subscript value for array in '//wstrng(:iend)
            ier=-1
            fval=0.0d0
         else                                                  ! legitimate subscripts to store at
            STEPTHRU: do i1033=2,n                             ! for each argument after the first one store the argument
               select case(wstrng2(:iend))                     ! select X array or Y array
                  case("xstore");x(istoreat)=args(i1033)
                  case("ystore");y(istoreat)=args(i1033)
               end select
               istoreat=istoreat+1                             ! increment location to store next value at
            enddo STEPTHRU
            fval=args(n)                                       ! last value stored will become current value
         endif
      endif
!=======================================================================------------------------------------------------------------
case("lle","llt","leq","lge","lgt","lne")
      if(iargs_type(1).eq.2.and.iargs_type(2).eq.2)then
         do i2020=1,n
            if(args(i2020).le.0.or.args(i2020).gt.size(values))then
               ier=-1
               mssge='unacceptable locations for strings encountered'
               goto 999
            endif
         enddo
         fval=FALSE ! assume false unless proven true
         i1=int(args(1))
         i2=int(args(2))
         ier=0
         select case (wstrng2(:iend))
         case("lle")
            if(values(i1).le.values(i2))fval=TRUE
         case("llt")
            if(values(i1).lt.values(i2))fval=TRUE
         case("leq") ! if any string matches the first
            do i410=2,n
               if(iargs_type(i410).ne.2)then     ! all parameters should be a string
                  ier=-1
                  mssge='non-string value encountered'
               elseif(values(i1).eq.values(int(args(i410)+0.5d0)))then
                  fval=TRUE
               endif
            enddo
         case("lge")
            if(values(i1).ge.values(i2))fval=TRUE
         case("lgt")
            if(values(i1).gt.values(i2))fval=TRUE
         case("lne")
            do i440=2,n
               fval=TRUE
               if(iargs_type(i440).ne.2)then     ! all parameters should be a string
                  ier=-1
                  mssge='non-string value encountered'
               elseif(values(i1).eq.values(int(args(i440)+0.5d0)))then
                  fval=FALSE
               endif
            enddo
         case default
            ier=-1
            mssge='internal error in funcs_ in lexical functions'
         end select
      else
         ier=-1
         mssge='lexical functions must have character parameters'
      endif
!=======================================================================------------------------------------------------------------
case("le","lt","eq","ge","gt","ne")
      fval=FALSE
      do i520=1,n
         if(iargs_type(i520).ne.0)then  ! this parameter was not a number
            ier=-1
            mssge='*logical* parameter was not a number'
            goto 999
         endif
      enddo
      if(n.eq.2.or.n.eq.3)then
         if(n.eq.3)then
            idig=int(args(3))
            if(idig.le.0.or.idig.gt.13)then
               mssge='*logical* precision must be between 1 and 13'
               ier=-1
               goto 999
            endif
            write(junout,'(a,3(g24.16e3,1x),i5)')'args=',args(1),args(2),args(3),idig
            write(*,g)trim(junout)
            arg1=round(args(1),idig)
            arg2=round(args(2),idig)
            write(junout,'(a,3(g24.16e3,1x),i5,1x,2(g24.16e3,1x))')'b. args=',args(1),args(2),args(3),idig,arg1,arg2
            write(*,g)trim(junout)
         else
            arg1=args(1)
            arg2=args(2)
         endif
         call stuff('LOGICAL1',arg1)
         call stuff('LOGICAL2',arg2)
         call stuff('STATUS',arg2-arg1)
         select case(wstrng2(:iend))
         case("le"); if(arg1.le.arg2)fval=TRUE
         case("lt"); if(arg1.lt.arg2)fval=TRUE
         case("eq"); if(arg1.eq.arg2)fval=TRUE
         case("ge"); if(arg1.ge.arg2)fval=TRUE
         case("gt"); if(arg1.gt.arg2)fval=TRUE
         case("ne"); if(arg1.ne.arg2)fval=TRUE
         case default
            ier=-1
            mssge='*logical* internal error in funcs_'
         end select
      else
         ier=-1
         mssge='*logical* must have 2 or 3 parameters'
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("ichar")
      if(n.ne.1)then
         mssge='*ichar* takes one parameter'
         ier=-1
      elseif(iargs_type(1).ne.2)then
         mssge='*ichar* parameter must be a string'
         ier=-1
      else
         fval=ichar(values(int(args(1)))(1:1))
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("$ge","$getenv") ! $getenv or $ge get system environment variable
   ii=int(args(1)+0.5d0)
   ilen=values_len(ii)
   if(ilen.ge.1)then
      call get_environment_variable(values(ii)(:ilen),ctmp)
      fval=len_trim(ctmp)
      if(fval.eq.0)then ! if value comes back blank and a non-blank default string is present, use it
         if(n.ge.2)then
            ii2=int(args(2)+0.5d0)
            ctmp=values(ii2)
            fval=values_len(ii2)
         endif
      endif
      fval=max(1.0d0,fval)
   else
      ctmp=' '
      fval=1.0d0
   endif
   ier=2
   iend=len_trim(ctmp)
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("if")
      if(args(1).eq. TRUE)then
        fval=args(2)
      else
        fval=args(3)
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("$if")                                      ! $if function
      ier=2                                      ! returning string
!     check that 2nd and 3rd are acceptable string variables, should do generically at name lookup time
      if(args(1).eq. TRUE)then
        ii=int(args(2))
      else
        ii=int(args(3))
      endif
      ctmp=values(ii)
      iend=values_len(ii)
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("in")                                                            ! in(lower_value,value,upper_value)
      fval=FALSE
      !=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      select case(n)
      !=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      case(2)                                                         ! if two parameters test {first - second}<epsilon
        if(iargs_type(1).eq.0.and.iargs_type(2).eq.0)then
           val=abs(args(1)-args(2))
           top=epsilon(0.0d0)
           bottom=-top
        else
         mssge='*in* parameters must be numeric'
         ier=-1
        endif
      !=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      case(3)                                                         ! if three parameters test if second between first and third
        if(iargs_type(1).eq.0.and.iargs_type(2).eq.0.and.iargs_type(3).eq.0)then
           bottom=args(1)
           val=args(2)
           top=args(3)
        else
         mssge='*in* parameters must be numeric'
         ier=-1
        endif
      !=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      case default
         mssge='*in* number of parameters not valid IN(LOWER_VALUE,VALUE,UPPER_VALUE)'
         ier=-1
      !=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      end select
      !=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      if(ier.ge.0) then
         if(val.ge.bottom.and.val.le.top)fval=TRUE
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("index")
      ii=int(args(1))
      iii=int(args(2))
      if(iargs_type(1).eq.2.and.iargs_type(2).eq.2)then ! if parameter was a string leave it alone
         iend1=values_len(ii)
         iend2=values_len(iii)
         fval=index(values(ii)(:iend1),values(iii)(:iend2))
      endif
      ier=0   ! flag that returning a number, not a string
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("len","len_trim")
      ii=int(args(1))
      iend1=values_len(ii)
      fval=len_trim(values(ii)(:iend1))
      ier=0   ! flag that returning a number, not a string
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("rand")                                                            ! random number
      select case (n)                                                   ! check number of parameters
      case (0)                                                          ! use default method
         itype=3
      case (1)                                                          ! determine user-specified method
         itype=int(args(1)+0.5d0)
         if(itype.lt.1.or.itype.gt.3)then
            itype=3
         endif
      case default
         mssge='illegal number of arguments for rand()'
         ier=-1
         itype=-1
      end select

      select case (itype)                                               ! select various methods
      case (-1)                                                         ! an error has already occurred
      case (2)                                                          ! standard Fortran function
         call random_number(harvest=fval)
      !!case default                                                      ! "Numerical Recipes" routine
      !!fval=ran_mod(ikeepran)
      case default
         call random_number(harvest=fval)
      end select
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("srand")                                                           ! seed random number sequence
      select case (n)                                                   ! check number of parameters
      case (1)                                                          ! no user-specified type
         itype=3                                                        ! use default method
      case (2)                                                          ! determine user-specified method
         itype=int(args(2)+0.5d0)                                         ! user-specified type
      case default                                                      ! call syntax error
         mssge='illegal number of arguments for srand()'
         ier=-1
      end select
      if(ier.eq.0)then
         ivalue=int(args(1)+0.5d0)                                           ! determine seed value
         select case (itype)                                               ! select various methods
         case (2)                                                          ! standard Fortran method
            call init_random_seed(ivalue)
         !!case (3)                                                          ! default is "Numerical Recipes" method
         !!   ikeepran=-abs(ivalue)
         !!   fval=ran_mod(ikeepran)                                         ! just setting seed; fval is a dummy here
         case default
            mssge='unknown type for srand()'
            ier=-1
         end select
         fval=ivalue
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("$f")                              ! $f(format,value) Using single format specifier, return string
      ier=2                             ! string will be returned
      if(n.eq.0)then
         ctmp=' '
      else
         ctmp=' '
         if(iargs_type(1).eq.2)then     ! if first field is a string
            ii=int(args(1))             ! get index into values() array
            iend1=values_len(ii)            ! maximum end is at end of string
            if(n.gt.1)fval=args(n)      ! get the real value
            write(ctmp,'('// values(ii)(:iend1)//')',iostat=ios)args(2:n)
            if(ios.ne.0)then
               ctmp='*'
               ier=-1
               mssge='*$f()* error writing value'
            endif
         endif
      endif
      iend=len_trim(ctmp)
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("$repeat")                              ! $repeat(string,count) concatenate string count times
      ier=2                                  ! string will be returned
      if(n.eq.0)then
         ctmp=' '
      else
         ctmp=' '
         if(iargs_type(1).eq.2)then     ! if first field is a string
            ii=int(args(1))             ! get index into values() array
            iend1=values_len(ii)        ! maximum end is at end of string
            if(n.gt.1)fval=args(n)      ! get the real value
            if(fval.lt.0)then
               ier=-1
               mssge='Argument NCOPIES of REPEAT intrinsic is negative'
            else
               ctmp=repeat(values(ii)(:iend1),nint(fval))
            endif
         else
            ier=-1
            mssge='*$repeat()* first value not string'
         endif
      endif
      iend=len_trim(ctmp)
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("$char")
      ier=2                                      ! return string
      if(n.eq.0)then
         ier=-1
         mssge='*$char* must have at least one parameter'
      else
         iend=0
         do i3030=1,n                            ! unlike FORTRAN, can take multiple characters and mix strings and numbers
            ii=int(args(i3030))
            if(iargs_type(i3030).eq.2)then       ! if parameter was a string leave it alone
               iend2=iend+values_len(ii)
               ctmp(iend+1:iend2)=values(ii)
               iend=iend2
            else                                 ! convert numeric ADE to a character
               iend=iend+1
               ctmp(iend:iend)=char(ii)
            endif
         enddo
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("$substr")                                  ! $substr(string,start,end)
      ier=2                                      ! return string
      if(n.eq.0)then
         ctmp=' '
      else
         ii=int(args(1))
         istart=1
         iend1=values_len(ii)                        ! maximum end is at end of string
         ctmp=' '
         if(iargs_type(1).eq.2)then
            if(n.gt.1)istart=min(max(1,int(args(2))),iend1)
            if(n.gt.2)iend1=max(min(int(args(3)),iend1),1)
            iend=iend1-istart+1
            ctmp(:iend)=values(ii)(istart:iend1)
         endif
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("$nstore","$xstore","$ystore")
      ier=2                                      ! return string
      if(n.lt.2)then
         ier=-1
         mssge='incorrect number of parameters for '//wstrng(:iend)
      else
         ivalue=int(args(1)+0.5d0)
!        $nstore function===>store $n(where_to_start,value1,value2,value3...)
!        ignore -entire- function call if a bad subscript reference was made
         if(ivalue.lt.1.or.ivalue+n-2.gt.ixyc_calc)then
           mssge='illegal subscript value for array in '//wstrng2(:iend)
           ier=-1
         else
            do i1066=ivalue,ivalue+n-2,1
               isub=i1066-ivalue+2
               select case(wstrng2(:iend))
               case("$nstore"); nc(i1066)=values(int(args(isub)))
               case("$xstore"); xc(i1066)=values(int(args(isub)))
               case("$ystore"); yc(i1066)=values(int(args(isub)))
               end select
            enddo
            ctmp=values(ivalue+n-2)
            iend=len_trim(ctmp) ! very inefficient
         endif
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("str","$str","$") ! "$str" appends numbers and strings into a new string
                   ! "str" converts string to number IF string is simple numeric value
      jend=0
      ctmp=' '
      do i1010=1,n
         istart=jend+1                                     ! where to start appended argument in output string
         if(iargs_type(i1010).eq.2)then                    ! this parameter was a string
            in=int(args(i1010))                            ! the value of a string argument is the subscript for where the string is
            jend=istart+values_len(in)-1                       ! where appended argument ends in output string
            ctmp(istart:jend)=values(in)(:values_len(in))
         elseif(iargs_type(i1010).eq.0)then                ! this parameter was a number
            if(args(i1010).ne.0)then
               call value_to_string(args(i1010),cnum,ilen,ier,fmt='(g23.16e3)',trimz=.true.) ! minimum of 23 characters required
               if(ier.ne.-1)then
                  ilen=max(ilen,1)
                  jend=istart+ilen-1
                  if(cnum(ilen:ilen).eq.'.')jend=jend-1    ! this number ends in a decimal
                  jend=max(jend,istart)
                  if(jend.gt.len(ctmp))then
                     write(*,g)'*funcs_* $str output string truncated'
                     jend=len(ctmp)
                  endif
                  ctmp(istart:jend)=cnum(:ilen)
               endif
            else                                           ! numeric argument was zero
               ctmp(istart:istart)='0'
               jend=jend+1
            endif
         else
            mssge='*funcs_* parameter to function $str not interpretable'
            ier=-1
         endif
      enddo
      if(ier.ge.0)then
         select case(wstrng2(:iend))
         case("$str","$")
             ier=2
         case("str")
             ier=0
             call a_to_d_(ctmp,fval,ier)                    ! str function
         case default
            mssge='*funcs_* internal error: should not get here'
            ier=-1
         end select
      endif
      iend=jend
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("$x","$y","$n")
      ier=2                                                  ! returning string
      ivalue=int(args(1)+0.5d0)
      if(ivalue.lt.1.or.ivalue.gt.ixyc_calc)then             ! if value not at least 1, or if not less than ixyc_calc, report it
        mssge='illegal subscript value for $x array'
        ier=-1
      else
         select case(wstrng2(:iend))
            case("$x");ctmp= xc(ivalue)
            case("$y"); ctmp= yc(ivalue)
            case("$n"); ctmp= nc(ivalue)
         end select
         iend=len_trim(ctmp) ! very inefficient
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("unusedf")
      fval=0.0d0
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("$l") ! $l lower(string)
      ier=2                                      ! returning string
      if(n.ne.1)then
         ctmp=' '
         ier=-1
         mssge='*$l* must have one parameter'
      else
         ctmp=lower(values(int(args(1)+0.5d0)))
         iend=len_trim(ctmp) ! very inefficient
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("$u")! $u upper(string)
      ier=2                                      ! returning string
      if(n.ne.1)then
         ctmp=' '
         ier=-1
         mssge='*$u* must have one parameter'
      else
         ctmp=upper(values(int(args(1)+0.5d0)))
         iend=len_trim(ctmp) ! very inefficient
      endif
!=======================================================================------------------------------------------------------------
case("c");      fval= myfunc(args,n)                          ! c(curve_number) or c(curve_number,index)
!=======================================================================------------------------------------------------------------
case("ownmode")                                               ! specify whether to look for substitute_subroutine(3f) routine
      if(n.eq.1.and.iargs_type(1).eq.0)then
         if(args(1).gt.0)then
            ownon=.true.
         else
            ownon=.false.
         endif
         fval= args(1)
      else
        mssge='*ownmode* illegal arguments'
        ier=-1
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("delimx")                ! 'delimx(istore,line,delimiters)  parse a string into $x array and return number of tokens
      if(n.ne.3)then                                                                     ! wrong number of parameters
         ier=-1
         mssge='incorrect number of parameters for '//wstrng(:iend)
      else
         if(iargs_type(2).ne.2)then
            mssge='*delimx* second parameter not a string'
           ier=-1
         else
            ctmp=values(int(args(2)+0.5d0))                                                ! string to parse
            if(iargs_type(3).ne.2)then
               mssge='*delimx* delimiter parameter not a string'
              ier=-1
            else
               ctmp2=values(int(args(3)+0.5d0))                                            ! delimiters
               if(iargs_type(1).ne.0)then
                  mssge='*delimx* first parameter not an index number'
                 ier=-1
               else
                  istore=int(args(1)+0.5d0)                                                ! where to start storing into $n array at
                  call delim(ctmp,['#NULL#'],ixyc_calc,icount,ibegin,iterm,ilen,ctmp2)
                  if(istore.lt.1.or.istore+n-2.gt.ixyc_calc)then  ! ignore entire function call if bad subscript reference was made
                     mssge='illegal subscript value for array in delim'
                     ier=-1
                  else
                     do i1060=1,icount
                        xc(istore)=ctmp(ibegin(i1060):iterm(i1060))
                        istore=istore+1
                     enddo
                     fval=icount  ! return number of tokens found
                     ier=0
                  endif
               endif
            endif
         endif
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("round")
      if(n.ne.2)then                                                           ! check number of parameters
        mssge='*funcs_* incorrect number of parameters in '//wstrng2(:iend)
        ier=-1
      elseif(.not.all(iargs_type(1:2).eq.0))then                                ! check type of parameters
        mssge='*funcs_* parameters not all numeric in '//wstrng2(:iend)
        ier=-1
      else                                                                      ! single numeric argument
         fval=round(args(1),int(args(2)))
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("ifdef")
      fval=-1
      if(n.ne.1)then                                             ! check number of parameters
         mssge='*ifdef* incorrect number of parameters in '//wstrng(:iend)
         ier=-1
      elseif(iargs_type(1).ne.2)then                             ! the parameter should be a name of a variable
         mssge='*ifdef* name not a string:'//wstrng(:iend)
         ier=-1
      else
         ii=int(args(1))                                         ! get index into values() array
         iend1=values_len(ii)                                        ! maximum end is at end of string
         if(values(ii)(1:1).eq.'$')then
            call locate(keys_q,values(ii)(:iend1),indexout,ierr) ! determine if the string variable name exists
         else
            call locate(keyr_q,values(ii)(:iend1),indexout,ierr) ! determine if the numeric variable name exists
         endif
         if(ierr.ne.0)then                                       ! unexpected error
            ier=-1
         elseif(indexout.gt.0)then                               ! found variable name
            fval=0
         else                                                    ! did not find variable name
            fval=-1
         endif
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("ye","year","mo","month","da","day","tz","timezone","ho","hour","mi","minute","se","second","ms","millisecond")
         icalen=1                                                       ! default value that is safe even if an error occurs
         !------------------------------------------
         call date_and_time(values=idarray)
         !------------------------------------------
         if(n.eq.0)then
            select case(wstrng2(:iend))                                 ! select desired subscript of value to return
               case("ye","year");         icalen=1                      ! year
               case("mo","month");        icalen=2                      ! month
               case("da","day");          icalen=3                      ! day
               case("tz","timezone");     icalen=4                      ! timezone
               case("ho","hour");         icalen=5                      ! hour
               case("mi","minute");       icalen=6                      ! minute
               case("se","second");       icalen=7                      ! second
               case("sd","millisecond");  icalen=8
               case default                                             ! report internal error if name was not matched
                  ier=-1
                  mssge='*calendar* internal error, unknown keyword'//wstrng2(:iend)
               end select
               fval=idarray(icalen)
         else
            ier=-1
            mssge='*calendar* parameters not allowed'
            fval=0.0d0
         endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("$mo")                             ! $mo(1-12) is "January, February, ... ")
      ctmp=''
      ier=2                             ! string will be returned
      if(n.lt.1)then                    ! $mo() use today
         call date_and_time(values=idarray)
         ival=idarray(2)
      elseif(n.eq.1)then                ! $mo(N) just index into month names
         ival=mod(int(args(1))-1,12)+1
         if(ival.le.0)ival=ival+12
      elseif(args(2).eq.1)then          ! $mo(YYYYMMDD,1) returns MM
         ival=int(args(1))
         ival=ival-((ival/10000)*10000) ! reduce to a four-digit value
         ival=ival/100                  ! keep two high digits out of the four
         ival=mod(ival-1,12)+1          ! ensure in range 1 to 12
         if(ival.le.0)ival=ival+12
      else
         ival=1
         ctmp='UNKNOWN'
         iend=7
         mssge='*$mo* parameter(s) not valid'
         ier=-1
      endif
      if(ctmp.eq.'')then
         ctmp=months(ival)
      endif
      iend=len_trim(ctmp(1:20))
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case default
      if(ownon)then
!        ==>wstrng(1:iend)=procedure name.
!        ==>iend=length of procedure name.
!        ==>args=array of ixy_calc elements containing procedure arguments.
!        ==>iargs_type=type of argument
!        ==>n=integer number of parameters
!        ==>x=array of ixy_calc x values
!        ==>y=array of ixy_calc y values
!        ==>ctmp is returned string if string function is called ( in which case fval is returned number of characters in ctmp)
         ier=0
         call mysub(wstrng(:iend),iend,args,iargs_type,n,fval,ctmp,ier)
!        <==fval=returned value to replace function call with
!        <=>ier=returned error flag.  Set to -1 if an error occurs.  Otherwise, user should leave it alone
         if(ier.eq.-1)then
         elseif(ier.eq.2)then
            iend=int(fval) ! string functions should return string length in fval
            if(fval.le.0)then
               mssge='*funcs_* bad length for returned string'
               ier=-1
            endif
         else
            ier=0
         endif
      else
        mssge='*funcs_* function not found: '//wstrng(:iend)
        ier=-1                           ! ya done blown it if you get here
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
end select
!-----------------------------------------------------------------------------------------------------------------------------------
999   continue             ! return based on value of ier
      select case(ier)
      case(2)   ! return string value
        call stufftok_(fval,wstrng,nchars,ctmp(:iend),iend,ier) ! add a new token variable and assign string to it
      case(0)   ! return numeric value
        call value_to_string(fval,wstrng,nchars,idum,fmt='(g23.16e3)',trimz=.true.) ! minimum of 23 characters required
      case(-1)  ! return error
      case default
         write(*,g)'*funcs_* unknown closing value ',ier
         ier=-1
      end select
end subroutine funcs_
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    stufftok_(3fp)- [M_calculator] add a new token variable and assign string to it
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##RETURNS
!!##EXAMPLE
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine stufftok_(fval,wstrng,nchars,string,iend,ier)

! ident_5="@(#) M_calculator stufftok_(3fp) add a new token variable and assign string to it"

real(kind=dp)          :: fval
character(len=*)       :: wstrng
integer                :: nchars
character(len=*)       :: string
integer                :: iend
integer                :: ier

character(len=5)       :: toknam
!-----------------------------------------------------------------------------------------------------------------------------------
   ktoken=ktoken+1                            ! increment the counter of strings found to get a place to store into
   nchars=5
   write(toknam,'(''$_'',i3.3)')ktoken        ! build a unique name for the token string found for this output string
   wstrng=toknam
   call stuffa(toknam,string(:iend),'')       ! cannot do this earlier or indexs from call that defined args could be wrong
   fval=0.0d0
   ier=2
   mssge=string(:iend)
end subroutine stufftok_
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    args_(3fp)- [M_calculator] given 'par1,par2,...' store non-parenthesized expression par(n) into a real or string array
!! (LICENSE:PD)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##RETURNS
!!##EXAMPLE
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine args_(line,ilen,array,itype,iarray,ier,mx)

! ident_6="@(#) M_calculator args_(3fp) given 'par1 par2 ...' store non-parenthesized expression par(n) into a real or string array"

!@ (#) record type of par(n) into itype()"
!@ (#) Commas are only legal delimiters. extra or redundant delimiters are ignored.

!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)           :: line      ! input string
integer,intent(in)                    :: ilen      ! length of input string
integer,intent(in)                    :: mx        ! up to mx par(i) will be extracted. if more found an error is generated.
real(kind=dp),intent(out)             :: array(mx)
integer,intent(out)                   :: itype(mx) ! itype=0 for number, itype=2 for string
integer,intent(out)                   :: iarray    ! number of parameters found
integer                               :: ier       ! ier=-1 if error occurs, ier undefined (not changed) if no error.
!-----------------------------------------------------------------------------------------------------------------------------------
integer :: icalc
integer :: icol
integer :: iend
integer :: ilook
integer :: istart
character(len=1),parameter :: delimc=','
character(len=icbuf_calc)  :: wstrng
!-----------------------------------------------------------------------------------------------------------------------------------
   iarray=0
   if(ilen.eq.0)then  ! check if input line (line) was totally blank
      return
   endif
!  there is at least one non-delimiter character in the command.
!  ilen is the column position of the last non-blank character
!  find next non-delimiter
   icol=1
   do ilook=1,mx,1
     do
        if(line(icol:icol).ne.delimc)then
           iarray=iarray+1
           istart=icol
           iend=index(line(istart:ilen),delimc)
           if(iend.eq.0)then   ! no delimiter left
              icalc=ilen-istart+1
              wstrng=line(istart:ilen)
              ier=0
              call expressions_(wstrng,icalc,array(iarray),ier)
              itype(iarray)=ier
              return
           else
              iend=iend+istart-2
              icalc=iend-istart+1
              wstrng=line(istart:iend)
              ier=0
              call expressions_(wstrng,icalc,array(iarray),ier)
              itype(iarray)=ier
              if(ier.eq.-1)return
           endif
           icol=iend+2
           exit
        else
           icol=icol+1
           if(icol.gt.ilen)return       ! last character in line was a delimiter, so no text left
        endif
     enddo
     if(icol.gt.ilen)return       ! last character in line was a delimiter, so no text left
   enddo
   write(mssge,'(a,i4,a)')'more than ',mx,' arguments not allowed'
   ier=-1
end subroutine args_
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!! expressions_(3fp) - [M_calculator] resolve a series of terms into a single value and restring
!! (LICENSE:PD)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##RETURNS
!!##EXAMPLE
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine expressions_(string,nchar,value,ier)

! ident_7="@(#) M_calculator expressions_(3fp) resolve a series of terms into a single value and restring"

character(len=*),intent(inout) :: string
integer,intent(inout)          :: nchar
real(kind=dp),intent(out)      :: value
integer,intent(out)            :: ier
character(len=icbuf_calc) :: dummy            ! no single term may be over (icbuf_calc) characters
integer :: ier2
integer :: iend
integer :: iendm
integer :: iendp
integer :: ista
integer :: istat
integer :: nchar2
real(kind=dp) :: temp
!-----------------------------------------------------------------------------------------------------------------------------------
                               !!!!! what happens if the returned string is longer than the input string?
      value=0.0d0              ! initialize sum value to be returned to 0
      if(nchar.eq.0) return    ! if this is a null string return
                 ! first cut at handling string variables. assuming, with little checking, that the only string expression
                 ! that can get here is a single variable name (or variable token) and that string variable names start with a $
                 ! and that the error flag should be set to the value 2 to indicate that a string, not a number, is being returned
                 ! for the 2 to get back, it must not be changed by this routine or anything it calls
      if(string(1:1).eq.'$')then
         call given_name_get_stringvalue_(string,ier)
         if(ier.eq.-1)return
         ier=2                 ! flag that a character string is being returned
!x       return
      endif
!x!!!!!
      ista=1                                        ! initialize the position of the unary sum operator for the current term
      if(index('#=',string(1:1)).ne.0)then          ! check if input string starts with a unary (+-) operator
        istat=2                                     ! a starting unary sum operator is present, so the first term starts in column 2
      else                                          ! input string does not start with a unary sum (-+) operator
        istat=1                                     ! no initial sum operator is present, so the first term starts in column 1
      endif
      do
         iendp=index(string(istat:nchar),'#')     ! find left-most addition operator
         iendm=index(string(istat:nchar),'=')     ! find left-most subtraction operator
         iend=min(iendp,iendm)                    ! find left-most sum (+-) operator assuming at least one of each exists
         if(iend.eq.0)iend=max(iendm,iendp)       ! if one of the sum operators is not remaining, find left-most of remaining type
         if(iend.eq.0)then                        ! if no more sum operators remain this is the last remaining term
            iend=nchar                            ! find end character of remaining term
         else                                     ! more than one term remains
            iend=iend+istat-2                     ! find end character position of this (left-most) term
         endif
         dummy=string(istat:iend)                 ! set string dummy to current(left-most) term
         nchar2=iend-istat+1                      ! calculate number of characters in current term
!        given that the current term ( dummy) is an optionally signed string containing only the operators **, * an / and no
!        parenthesis, reduce the string to a single value and add it to the sum of terms (value). do not change the input string.
         call pows_(dummy,nchar2,ier)            ! evaluate and remove ** operators and return the altered string (dummy)
         if(ier.eq.-1) return                     ! if an error occurred, return
         call factors_(dummy,nchar2,temp,ier)       ! evaluate and remove * and / operators, return the evaluated -value- temp
         if(ier.eq.-1)return                      ! if an error occurred, return
         if(string(ista:ista).eq.'=')then         ! if term operator was a subtraction, subtract temp from value
            value=value-temp
         else                                     ! operator was an addition (+) , add temp to value
!           if first term was not signed, then first character will not be a subtraction, so addition is implied
            value=value+temp
         endif
         ista=iend+1                      ! calculate where next sum operator (assuming there is one) will be positioned in (string)
         istat=ista+1                     ! calculate where beginning character of next term will be (if another term remains)
         if(iend.ne.nchar)then
            if(istat.gt.nchar)then                ! a trailing sum operation on end of string
               ier=-1
               mssge='trailing sum operator'
               return
            endif
         ! if last term was not the end of (string) terms remain. keep summing terms
         else
            exit
         endif
      enddo
      call value_to_string(value,string,nchar,ier2,fmt='(g23.16e3)',trimz=.true.) ! convert sum of terms to string and return
      if(ier2.lt.0)ier=ier2
end subroutine expressions_
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!! (LICENSE:PD)
!!##SYNOPSIS
!!
!!   subroutine pows_(wstrng,nchar,ier)
!!
!!    character(len=*),intent(inout) :: wstrng
!!    integer,intent(inout)          :: nchar
!!    integer                        :: ier
!!##DESCRIPTION
!!    given an unparenthesized string of form:
!!
!!       stringo opo fval1 ** fval2 opo2 stringo2
!!
!!    where opo is a preceding optional operator from set /,* and
!!    stringo is the string that would precede opo when it exists,
!!    and opo2 is an optional trailing operator from set /,*,**
!!    and stringo2 the string that would follow op2 when it exists,
!!    evaluate the expression fval1**fval2 and restring it; repeating
!!    from left to right until no power operators remain in the string
!!    or an error occurs
!!
!!##OPTIONS
!!    wstrng  input string returned with power operators evaluated
!!    nchar   input length of wstrng, returned corrected for new wstrng returned.
!!    ier     error status
!!
!!
!!##RETURNS
!!##EXAMPLE
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine pows_(wstrng,nchar,ier)

! ident_8="@(#) M_calculator pows_(3fp) expand power functions in a string working from left to right"

character(len=*),intent(inout) :: wstrng    ! input string returned with power operators evaluated
integer,intent(inout)          :: nchar     ! input length of wstrng, returned corrected for new wstrng returned.
integer                        :: ier       ! error status
character(len=iclen_calc)     :: tempch
character(len=icbuf_calc)      :: dummy
character(len=1)               :: z
real(kind=dp)                  :: fval1
real(kind=dp)                  :: fval2
integer                        :: i
integer                        :: id2
integer                        :: idum
integer                        :: im2
integer                        :: ip        ! position of beginning of first ** operator
integer                        :: ip2
integer                        :: iright    ! end of fval2 string
integer                        :: iz        ! beginning of fval1 string
integer                        :: nchart
!-----------------------------------------------------------------------------------------------------------------------------------
      INFINITE: do
!        find first occurrence of operator, starting at left and moving right
         ip=index(wstrng(:nchar),'**')
         if(ip.eq.0) then
            exit INFINITE
         elseif(ip.eq.1) then
            ier=-1
            mssge='power function "**" missing exponentiate'
            exit INFINITE
         elseif((ip+2).gt.nchar) then
            ier=-1
            mssge='power function "**" missing power'
            exit INFINITE
         endif
!
!        find beginning of fval1 for this operator. go back to
!        beginning of string or to any previous * or / operator
         FINDVAL: do i=ip-1,1,-1
            iz=i
            z=wstrng(i:i)
               if(index('*/',z).ne.0)then  ! note that use of index function was faster than .eq. on cyber
                  iz=iz+1
                  goto 11
               endif
         enddo FINDVAL
         iz=1
11       continue
         if(ip-iz.eq.0)then
           ier=-1
           mssge='operator / is beside operator **'
           exit INFINITE
         endif
!
!        now isolate beginning and end of fval2 string for current operator
!        note that looking for * also looks for ** operator, so checking
!        for * or / or ** to right
!
         im2=index(wstrng((ip+2):nchar),'*')
         id2=index(wstrng((ip+2):nchar),'/')
         ip2=min0(im2,id2)
         if(ip2.eq.0)ip2=max0(im2,id2)
         if(ip2.eq.0)then
           iright=nchar
         elseif(ip2.eq.1)then
           ier=-1
           mssge='two operators from set [*/**] are side by side'
           exit INFINITE
         else
           iright=ip2+ip
         endif
         call a_to_d_(wstrng(iz:ip-1),fval1,ier)
         if(ier.eq.-1)then
            exit INFINITE
         endif
         call a_to_d_(wstrng(ip+2:iright),fval2,ier)
         if(ier.eq.-1)then
            exit INFINITE
         endif
         if(fval1.lt.0.0d0)then
!           this form better/safe? if(abs( fval2-int(fval2)/fval2).le..0001)
            if(fval2-int(fval2).eq.0.0d0)then
               fval1=fval1**int(fval2)
            else
               mssge='negative to the real power not allowed'
               ier=-1
               exit INFINITE
            endif
         else
            fval1=fval1**fval2
         endif
         call value_to_string(fval1,tempch,nchart,idum,fmt='(g23.16e3)',trimz=.true.) ! minimum of 23 characters required
!        place new value back into string and correct nchar.
!        note that not checking for nchar greater than (icbuf_calc)
!        in dummy or greater than len(wstrng).
         if(iz.eq.1.and.iright.eq.nchar)then      ! there was only one operator and routine is done
            dummy=tempch(1:nchart)
            nchar=nchart
         else if(iz.eq.1)then                     ! iz was 1, but iright was nchar so
            dummy=tempch(1:nchart)//wstrng(iright+1:nchar)
            nchar=nchart+nchar-(iright+1)+1
         else if(iright.eq.nchar)then             ! iz was not 1, but iright was nchar so
            dummy=wstrng(1:iz-1)//tempch(1:nchart)
            nchar=(iz-1)+nchart
         else                                     ! iz was not 1, and iright was not nchar so
            dummy=wstrng(1:iz-1)//tempch(1:nchart)//wstrng(iright+1:nchar)
            nchar=(iz-1)+nchart+(nchar-(iright+1)+1)
         endif
         wstrng=dummy
      enddo INFINITE
end subroutine pows_
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!! (LICENSE:PD)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##RETURNS
!!##EXAMPLE
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine factors_(wstrng,nchr,fval1,ier)

! ident_9="@(#) M_calculator factors_(3fp) reduce unparenthesized string with only * and / operators to val"

!
!     The input string is unaltered. for any single pass thru the routine, the string structure is assumed to be:
!         fval1 op fval2 op fval op fval op fval op fval
!     where no blanks are in the string (only significant if string structure is bad) and the only operators are * or /.
!     working from left to right:
!       1. locate and place into a real variable the fval1 string
!       2. if one exists, locate and place into a real variable the fval2 string
!       3. perform the indicated operation between fval1 and fval2
!          and store into fval1.
!       3. repeat steps 2 thru 4 until no operators are left or
!          an error occurs.
!
!     nchr   = the position of the last non-blank character in the input string wstrng
!     ip     = the position of the current operator to be used.
!              to the left of this is the fval1 string.
!     iright = the position of the last character in the fval2 string.
!     wstrng = the input string to be interpreted.
!       ier  = is a flag indicating whether an error has occurred
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*) :: wstrng
real(kind=dp)    :: fval1
real(kind=dp)    :: fval2
integer          :: id
integer          :: id2
integer          :: ier
integer          :: im
integer          :: im2
integer          :: ip
integer          :: ip2
integer          :: iright
integer          :: nchr
!-----------------------------------------------------------------------------------------------------------------------------------
      if((nchr).eq.0)then
         ier=-1
         mssge='trying to add/subtract a null string'
         return
      endif
!     find position of first operator
      im=index(wstrng(:nchr),'*')
      id=index(wstrng(:nchr),'/')
!     ip should be the position of the left-most operator
      ip=min0(im,id)
!     if one or both of the operators were not present, then
!     either im or id (or both) are zero, so look for max
!     instead of min for ip
      if(ip.eq.0) ip=max0(im,id)
      if( ip.eq.0 )then
!        no operator character (/ or *) left
         call a_to_d_(wstrng(1:nchr),fval1,ier)
         return
      elseif (ip.eq.1)then
!        if no string to left of operator, have a bad input string
         ier=-1
         mssge='first factor or quotient for "*" or "/" missing or null'
         return
      endif
!     convert located string for fval1 into real variable fval1
      call a_to_d_(wstrng(1:ip-1),fval1,ier)
      if(ier.eq.-1)return
      do
         if(ip.eq.nchr)then
!          if no string to left of operator, have a bad input string
           ier=-1
           mssge='second factor or quotient for "*" or "/" missing or null'
           return
         endif
!        locate string to put into fval2 for current operator by starting just to right of operator and ending at end of current
!        string or at next operator note that because of previous checks we know there is something to right of the operator.
         im2=index(wstrng((ip+1):nchr),'*')
         id2=index(wstrng((ip+1):nchr),'/')
         ip2=min0(im2,id2)
         if(ip2.eq.0)ip2=max0(im2,id2)
         if(ip2.eq.0)then
            iright=nchr
         elseif(ip2.eq.1)then
            ier=-1
            mssge='two operators from set [*/] are side by side'
            return
         else
            iright=ip2+ip-1
         endif
!        place located string for fval2 into real variable fval2
         call a_to_d_(wstrng(ip+1:iright),fval2,ier)
         if(ier.eq.-1)return
!        do specified operation between fval1 and fval2
         if(wstrng(ip:ip).eq.'*') then
            fval1=fval1*fval2
         else if(fval2.eq.0) then
            ier=-1
            mssge='division by zero'
            return
         else
            fval1=fval1/fval2
         endif
         if(iright.eq.nchr)return
         ip=iright+1
      enddo
end subroutine factors_
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!     a_to_d_(3f) - [M_calculator] returns a double precision value from a numeric character string specifically for M_calculator(3fm)
!!     (LICENSE:PD)
!!##SYNOPSIS
!!
!!   subroutine a_to_d_(chars,rval,ierr)
!!
!!    character(len=*),intent(in) :: chars
!!    doubleprecision,intent(out) :: rval
!!    integer,intent(out)         :: ierr
!!
!!##DESCRIPTION
!!    Convert a string representing a numeric scalar value to a numeric value, specifically
!!    for the M_calculator(3fp) module.Works with any g-format input, including integer, real, and exponential forms.
!!
!!       1. if chars=? set rval to value stored as current value, return.
!!       2. if the string starts with a $ assume it is the name of a
!!          string variable or token and return its location as a doubleprecision number.
!!       3. try to read string into a doubleprecision value. if successful, return.
!!       4. if not interpretable as a doubleprecision value, see if it is a
!!          defined variable name and use that name's value if it is.
!!       5. if no value can be associated to the string and/or if
!!          an unexpected error has occurred, set error flag and
!!          error message and set rval to zero and return.
!!       6. note that blanks are treated as null, not zero.
!!
!!##OPTIONS
!!      chars  is the input string
!!      rval   is the doubleprecision output value
!!      ierr   0 if no error occurs
!!
!!##EXAMPLE
!!
!!
!!##VERSION
!!       o 07/15/1986  J. S. Urban
!!       o 12/28/1987  modified to specify bn in formats for reads. vax
!!                    defaults to zero-fill on internal files.
!!       o 12/22/2016  Changed to generate man(1) pages via prep(1).
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine a_to_d_(chars,rval8,ierr)

! ident_10="@(#) M_calculator a_to_d_(3f) returns a real value rval8 from a numeric character string chars."

! CAREFUL: LAST is in GLOBAL, but can be read from when passed to this routine as CHARS. DO NOT CHANGE CHARS.
character(len=*),intent(in) :: chars
character(len=:),allocatable :: chars_local
real(kind=dp),intent(out)   :: rval8
integer,intent(out)         :: ierr
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=13)           :: frmt
integer                     :: ier
integer                     :: indx
integer                     :: ioerr
!-----------------------------------------------------------------------------------------------------------------------------------
   ioerr=0
   chars_local=trim(adjustl(chars))//' ' ! minimum of one character required
   if(chars_local.eq.'?')then       ! if string is a (unsigned) question mark, use value returned from last completed calculation
     !x!read(last,'(bn,g512.40)',iostat=ioerr,err=9991)rval8  ! assuming cannot get a read error out of reading last
     write(frmt,101)len(last)                                 ! build a format statement to try and read the string as a number with
     chars_local=trim(last)//repeat(' ',512)                  ! kludge: problems if string is not long enough for format
     read(chars_local,fmt=frmt,iostat=ioerr,err=9991)rval8    ! try and read the string as a number
   elseif('$'.eq.chars_local(1:1))then                ! string is a string variable name
      call locate(keys_q,chars_local,indx,ier)        ! try to find the index in the character array for the string variable
      if(indx.le.0)then                         ! if indx is not .gt. 0 string was not a variable name
         ierr=-1
      mssge='undeclared string variable '//chars_local
      else
         rval8=real(indx)   ! set value to position of string in the string array
         !!!! flag via a value for ierr that a string, not a number, has been found
      endif
      return
   ! no error on read on Sun on character string as a number, so make sure first character not numeric and try as variable name
   elseif(index('0123456789.-+',chars_local(1:1)).eq.0)then   ! does not start with a numeric character. try as a variable name
      call locate(keyr_q,chars_local,indx,ier)
      if(indx.le.0)then                                 ! if indx is not .gt. 0 string was not a variable name
         ierr=-1
       mssge='*a_2_d_* undeclared variable ['//chars_local//']'
      else
         rval8=values_d(indx)
      endif
      return
   else                            ! string is a number or a numeric variable name that starts with a numeric character
     write(frmt,101)len(chars_local)                       ! build a format statement to try and read the string as a number with
101  format( '(bn,g',i5,'.0)' )
chars_local=chars_local//repeat(' ',512)                   ! kludge: problems if string is not long enough for format
     read(chars_local,fmt=frmt,iostat=ioerr,err=999)rval8  ! try and read the string as a number
     chars_local=trim(chars_local)
   endif
   return                                ! string has successfully been converted to a number
9991  continue                           ! string could not be read as number,so try as variable name that starts with number
999   continue                           ! string could not be read as number,so try as variable name that starts with number
   rval8=0.0d0
   indx=0
   !  either here because of a read error (too big, too small, bad characters in string) or this is a variable name
   !  or otherwise unreadable.
   !!!!! look carefully at what happens with a possible null string
   call locate(keyr_q,chars_local,indx,ier)
   if(indx.le.0)then                             ! if indx is not .gt. 0 string was not a variable name
      mssge='bad variable name or unusable value = '//chars_local
      ierr=-1
   else
      rval8=values_d(indx)
   endif
end subroutine a_to_d_
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    squeeze_ - [M_calculator] change +-[] to #=(),replace strings with placeholders,delete comments
!!    (LICENSE:PD)
!!
!!##DESCRIPTION
!!    Remove all blanks from input string and return position of last non-blank character in nchars using imax as the highest
!!    column number to search in. Return a zero in nchars if the string is blank.
!!
!!    replace all + and - characters with the # and = characters which will be used to designate + and - operators, as opposed to
!!    value signs.
!!
!!    replace [] with ()
!!
!!    remove all strings from input string and replace them with string tokens and store the values for the string tokens.
!!    assumes character strings are (iclen_calc) characters max.
!!    if string is delimited with double quotes, the double quote character may be represented inside the string by
!!    putting two double quotes beside each other ("he said ""greetings"", i think" ==> he said "greetings", i think)
!!
!!  !!!! if an equal sign is followed by a colon the remainder of the input line is placed into a string as-is
!!  !!!! without the need for delimiting it. ($string1=: he said "greetings", i think ==> he said "greetings", i think)
!!
!!    anything past an # is considered a comment and ignored
!!
!!    assumes length of input string is less than (icbuf_calc) characters
!!
!!    if encounters more than one equal sign, uses right-most as the
!!    end of variable name and replaces others with & and makes a
!!    variable name out of it (ie a=b=10 ===> a&b=10)
!!
!!  !!!!the length of string could actually be increased by converting quoted strings to tokens
!!
!!  !!!!maybe change this to allow it or flag multiple equal signs?
!!
!!  !!!!no check if varnam is a number or composed of characters
!!  !!!!like ()+-*/. . maybe only allow a-z with optional numeric
!!  !!!!suffix and underline character?
!!
!!  !!!!variable names ending in letter e can be confused with
!!  !!!!e-format numbers (is 2e+20 the variable 2e plus 20 or
!!  !!!!the single number 200000000000000000000?). to reduce
!!  !!!!amount of resources used to check for this, and since
!!  !!!!words ending in e are so common, will assume + and -
!!  !!!!following an e are part of an e-format number if the
!!  !!!!character before the e is a period or digit (.0123456789).
!!  !!!!and won't allow variable names of digit-e format).
!!
!!  !!!!make sure variable called e and numbers like e+3 or .e+3 are handled satisfactorily
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine squeeze_(string,imax,nchars,varnam,nchar2,ier)

! ident_11="@(#) M_calculator squeeze_(3fp) change +-[] to #=() replace strings with placeholders delete comments"

integer, parameter                      :: ilen=(icbuf_calc)+2
character(len=*)                        :: string
integer                                 :: imax
integer,intent(out)                     :: nchars
character(len=icname_calc),intent(out)  :: varnam
integer,intent(out)                     :: nchar2
integer,intent(out)                     :: ier
character(len=ilen)                     :: dummy
character(len=1)                        :: back1
character(len=1)                        :: back2
integer                                 :: iplace
character(len=1)                        :: currnt
character(len=iclen_calc)               :: ctoken
!!character(len=10),parameter             :: list  =' +-="#[]{}'  ! list of special characters
!!character(len=10),parameter             :: list2 =' #=&  ()()'  ! list of what to convert special characters too when appropriate
character(len=5)                        :: toknam
integer                                 :: i10
integer                                 :: i20
integer                                 :: idum
integer                                 :: ilook
integer                                 :: indx
integer                                 :: instring
integer                                 :: ipoint
integer                                 :: ivar
integer                                 :: kstrln
!-----------------------------------------------------------------------------------------------------------------------------------
!  keep track of previous 2 non-blank characters in dummy for when trying to distinguish between e-format numbers
!  and variables ending in e.
   back1=' '
   back2=' '
   varnam=' '                   ! initialize output variable name to a blank string
   ivar=0
   nchar2=0
   nchars=0                     ! the position of the last non-blank character in the output string (string)
   dummy(1:2)='  '
!-----------------------------------------------------------------------------------------------------------------------------------
!  instead of just copy string to buffer, cut out rows of sign operators
!  dummy(3:)=string
   dummy=' '
   idum=3
   instring=0
   do i10=1,len_trim(string)
      ! if adjacent sign characters skip new character and maybe change sign of previous character
      if(string(i10:i10).eq.'"'.and.instring.eq.0 )then   ! starting a string
         instring=1
      elseif(string(i10:i10).eq.'"'.and.instring.eq.1)then ! ending a string
         instring=0
      endif
      if(instring.ne.1)then
         if(string(i10:i10).eq.'+')then                 ! if found a + look to see if previous a + or -
            if(dummy(idum-1:idum-1).eq.'+')then         ! last character stored was also a sign (it was +)
               cycle                                    ! skip because ++ in a row
            elseif(dummy(idum-1:idum-1).eq.'-')then     ! skip -+ and just leave -
               cycle
            endif
         elseif(string(i10:i10).eq.'-')then             ! last character stored was also a sign (it was -)
            if(dummy(idum-1:idum-1).eq.'+')then         ! +- in a row
               dummy(idum-1:idum-1)='-'                 ! change sign of previous plus
               cycle                                    ! skip because +- in a row
            elseif(dummy(idum-1:idum-1).eq.'-')then     ! skip but change sign of previous
               dummy(idum-1:idum-1)='+'                 ! change -- to +
               cycle
            endif
         endif
      endif
      ! character not skipped
      dummy(idum:idum)=string(i10:i10)            ! simple copy of character
      idum=idum+1
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   string=' '
   ipoint=2                                       ! ipoint is the current character pointer for (dummy)
   ktoken=0                                       ! initialize the number of strings found in this string
   BIG: do ilook=1,imax
      ipoint=ipoint+1                             ! move current character pointer forward
      currnt=dummy(ipoint:ipoint)                 ! store current character into currnt
      select case(currnt)            ! check to see if current character has special meaning and requires processing ' +-="#[]{}'
!-----------------------------------------------------------------------------------------------------------------------------------
      case(" ")                                   ! current is a blank not in a string. ignore it
         cycle BIG
!-----------------------------------------------------------------------------------------------------------------------------------
      case("+")                                      ! current is a plus
         if(back1.eq.'e'.or.back1.eq.'E')then        ! if previous letter was an e it could be e-format sign or operator.
!           note not using dummy directly, as it may contain blanks letter before +- was an e. must decide if the +- is part of
!           an e-format number or intended to be the last character of a variable name.
!!!!!       what is effect on a---b or other +- combinations?
            ! if letter before e is not numeric this is a variable name and - is an operator
            if(index('0123456789.',back2).eq.0)then
              currnt="#"                        ! no digit before the e, so the e is the end of a variable name
            else                                ! digit before e, so assume this is number and do not change +- to #= operators
            endif
         else
            currnt="#"                          ! previous letter was not e, so +- is an operator so change +- to #= operators
         endif
!-----------------------------------------------------------------------------------------------------------------------------------
      case("-")                                      ! current is a minus
         if(back1.eq.'e'.or.back1.eq.'E')then        ! if previous letter was an e it could be e-format sign or operator.
!           note not using dummy directly, as it may contain blanks letter before +- was an e. must decide if the +- is part of
!           an e-format number or intended to be the last character of a variable name.
!!!!!       what is effect on a---b or other +- combinations?
            ! if letter before e is not numeric this is a variable name and - is an operator
            if(index('0123456789.',back2).eq.0)then
              currnt="="                       ! no digit before the e, so the e is the end of a variable name
            else                               ! digit before e, so assume this is number and do not change +- to #= operators
            endif
         else
            currnt="="                         ! previous letter was not e, so +- is an operator so change +- to #= operators
         endif
!-----------------------------------------------------------------------------------------------------------------------------------
      case("=")                                      ! current is a plus or minus
         currnt="&"
         ivar=nchars+1                               ! ivar is the position of an equal sign, if any
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      case ("{", "[")
      currnt='('                                     ! currnt is [ or { . Replace with (
      case ("}", "]")
      currnt=')'                                     ! currnt is ] or }, . Replace with )
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      case("#")                                      ! any remainder is a comment
         exit
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      case('"')                                   ! if character starts a quoted string, extract it and replace it with a token
!     figure out length of string, find matching left double quote, reduce internal "" to "
      kstrln=0                                    ! initialize extracted string length
      ctoken=' '                                  ! initialize extracted string
      do i20 = ipoint+1,imax+2                    ! try to find a matching double quote to the right of the first
         ipoint=ipoint+1
         if(dummy(ipoint:ipoint).eq.'"')then         !!!!! caution : could look at dummy(imax+1:imax+1)
            if(dummy(ipoint+1:ipoint+1).ne.'"')then  ! this is the end of the string
               goto 30
            else                                     ! this is being used to try and represent an internal double-quote
               kstrln=kstrln+1                               ! determine length of string to remove
               ctoken(kstrln:kstrln)=dummy(ipoint:ipoint)    ! store the character into the current string storage
               ipoint=ipoint+1
            endif
         else                                             ! this is an internal character of the current string
            kstrln=kstrln+1                               ! determining length of string to remove
            ctoken(kstrln:kstrln)=dummy(ipoint:ipoint)    ! store the character into the current string storage
         endif
      enddo
      ier=-1                                         ! if you get here an unmatched string delimiter (") has been detected
      mssge='unmatched quotes in a string'
      return
30    continue
!!!!! check that current token string is not over (iclen_calc) characters long . what about the string "" or """" or """ ?
      ktoken=ktoken+1                             ! increment the counter of strings found
      write(toknam,'(''$_'',i3.3)')ktoken         ! build a unique name for the token string found for this input string
      nchars=nchars+1                             ! increment counter of characters stored
      string(nchars:nchars+4)=toknam              ! replace original delimited string with its token
      nchars=nchars+4
!                                                    store the token name and value in the string variable arrays
      call locate(keys_q,toknam,indx,ier)         ! determine storage placement of the variable and whether it is new
      if(ier.eq.-1)return
      iplace=iabs(indx)
      if(indx.le.0)then                           ! check if the token name needs added or is already defined
         call insert(keys_q,toknam, iplace)   ! adding the new variable name to the variable name array
         call insert(values,' '   , iplace)
         call insert(values_len,0     , iplace)
      endif
      call replace(values,ctoken(:kstrln),iplace)          ! store a defined variable's value
      call replace(values_len,kstrln,iplace)                   ! store length of string
!!!!! note that reserving variable names starting with $_ for storing character token strings
      cycle BIG
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      case default                                   ! current is not one of the special characters in list
      end select
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
                                                  ! for all but blank characters and strings
      back2=back1
      back1=currnt
      nchars=nchars+1
      string(nchars:nchars)=currnt
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   enddo BIG
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
!  end of string or hit beginning of comment

   if(ivar.ne.0)then                   ! check to see if a variable name was defined:
   nchar2=ivar-1                       ! variable was declared nchar2 is the position of the last character in the variable name
     if(nchar2.gt.20)then
      ier=-1
      mssge='new variable names must be 20 characters long or less'
     else if(nchar2.eq.0)then
      ier=-1
      mssge='input starts with =; cannot define a null variable name'
     else                              ! split up variable name and expression
                                       ! legal length variable name
       if(index('eE',string(nchar2:nchar2)).ne.0.and.nchar2.ne.1)then ! could be an unacceptable variable name
           if(index('0123456789',string(nchar2-1:nchar2-1)).ne.0)then
!            an unacceptable variable name if going to avoid conflict with
!            e-format numbers in a relatively straight-forward manner
             mssge='variable names ending in digit-e not allowed'
             ier=-1
           endif
        endif
        dummy=string
        varnam=dummy(1:ivar-1)
        if(nchars.ge.ivar+1)then
           string=dummy(ivar+1:nchars)
        else
           string=' '
        endif
        nchars=nchars-ivar
     endif
   endif
end subroutine squeeze_
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!       [M_calculator] given_name_get_stringvalue_(3fp) - return associated value for variable name"
!!       (LICENSE:PD)
!!##SYNOPSIS
!!
!!   subroutine given_name_get_stringvalue_(chars,ierr)
!!
!!    character(len=*),intent(in)  :: chars
!!    integer,intent(out)          :: ierr
!!##DESCRIPTION
!!       return the actual string when given a string variable name or token
!!       the returned string is passed thru the message/string/error GLOBAL variable
!!##OPTIONS
!!       CHARS
!!       IER     ierr is set and returned as
!!
!!                 -1  an error occurs
!!                  2  a string is returned
!!##RETURNS
!!       MSSGE  when successful the variable value is returned through the global variable MSSGE
!!
!!##EXAMPLE
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine given_name_get_stringvalue_(chars,ierr)

! ident_12="@(#) M_calculator given_name_get_stringvalue_(3fp) return associated value for variable name"

!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)  :: chars
integer,intent(out)          :: ierr
!-----------------------------------------------------------------------------------------------------------------------------------
   integer                      :: index
!-----------------------------------------------------------------------------------------------------------------------------------
   ierr=0
   index=0
   call locate(keys_q,chars,index,ierr)
   if(ierr.eq.-1) then
   elseif(index.le.0)then
      ierr=-1
!!!!  what if len(chars) is 0? look carefully at what happens with a possible null string
      mssge=' variable '//trim(chars)//' is undefined'
   else
      ierr=2
      mssge=values(index)
   endif
end subroutine given_name_get_stringvalue_
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    stuff(3f) - [M_calculator] directly store value into calculator dictionary for efficiency
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine stuff(varnam,val,ioflag)
!!
!!    class(*),intent(in)         :: varnam
!!    character(len=*),intent(in) :: val
!!    integer,intent(in),optional :: ioflag
!!
!!##DEFINITION
!!    breaking the rule of only accessing the calculator thru calculator(3f):
!!
!!    a direct deposit of a value into the calculator assumed to
!!    be used only by friendly calls, for efficiency and to avoid
!!    problems with recursion if a routine called by the calculator
!!    in substitute_subroutine(3f) wants to store something back into the calculator
!!    variable table
!!
!!    Normally values are stored or defined in the calculator module
!!    M_calculator(3fm) using the calculator(3f) routine or the convenience
!!    routines in the module M_calculator(3fm). For efficiency when
!!    large numbers of values require being stored the stuff(3f) procedure
!!    can be used to store numeric values by name in the calculator
!!    dictionary.
!!
!!    breaking the rule of only accessing the calculator thru calculator(3f):
!!
!!    stuff(3f) is assumed to only be used when needed for efficiency and to
!!    avoid problems with recursion if a routine called by the calculator
!!    in substitute_subroutine(3f) wants to store something back into the
!!    calculator variable table.
!!
!!##OPTIONS
!!    varnam  name of calculator variable to define or replace val
!!    numeric value to associate with the name VARNAME. May be
!!            integer, real, or doubleprecision.
!!    ioflag  optional flag to use with journal logging. This string is
!!            passed directly to M_framework__journal::journal(3f)
!!            as the first parameter. The default is to not log the
!!            definitions to the journal(3f) command if this parameter is
!!            blank or not present.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_stuff
!!    use M_calculator, only : stuff, dnum0
!!    implicit none
!!    doubleprecision :: value
!!       call stuff('A',10.0)
!!       call stuff('PI',3.1415926535897932384626433832795)
!!       value=dnum0('A*PI')
!!       write(*,*)value
!!    end program demo_stuff
!!
!!   Expected result:
!!
!!    31.415926535897931
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine stuff(varnam,value,ioflag)

! ident_14="@(#) M_calculator stuff(3fp) pass key value and integer|real|doubleprecision value to dictionary(3f) as doubleprecision"

character(len=*),intent(in)           :: varnam        ! variable name to add or replace value of
class(*),intent(in)                   :: value
character(len=*),intent(in),optional  :: ioflag

real(kind=dp)                         :: val8          ! input value to store
character(len=:),allocatable          :: varnam_local  ! some trouble with variable length character strings on some machines
integer                               :: ierr
integer                               :: index
integer                               :: istart
!-----------------------------------------------------------------------------------------------------------------------------------
   varnam_local=adjustl(trim(varnam))//' '                      ! remove leading spaces but make sure at least one character long
   if(varnam_local(1:1).eq.'$')then                             ! add new variable to numeric value dictionary at specified location
      mssge='*stuff* numeric variable names must not start with a $'
      ierr=-1
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ierr=0
   call locate(keyr_q,varnam_local,index,ierr)
   istart=iabs(index)
   if(index.le.0)then   ! add entry to dictionary
      call insert(keyr_q,varnam_local,istart)
      call insert(values_d,0.0d0,istart)
   endif

   select type(value)
    type is (integer);         val8=dble(value)
    type is (real);            val8=dble(value)
    type is (doubleprecision); val8=value
   end select
   call replace(values_d,val8,istart)
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(ioflag))then                    ! display values with an assumed variable length of 20 characters so get neat columns
      write(*,g)ioflag,varnam_local//'=',val8
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine stuff
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!     stuffa(3f) - [M_calculator] directly store a string into calculator
!!     variable name table
!!     (LICENSE:PD)
!!##SYNOPSIS
!!
!!   subroutine stuffa(varnam,string,ioflag)
!!
!!    character(len=*),intent(in)          :: varnam
!!    character(len=*),intent(in)          :: string
!!    character(len=*),intent(in),optional :: ioflag
!!
!!##DEFINITION
!!    Breaking the rule of only accessing the calculator thru calculator(3f):
!!
!!    a direct deposit of a value into the calculator assumed to be used
!!    only by friendly calls, for efficiency and to avoid problems with
!!    recursion if a routine called by the calculator in JUOWN1(3f) wants
!!    to store something back into the calculator
!!    variable table.
!!
!!##OPTIONS
!!    varnam    variable name to create or replace in calculator module
!!    string    string to associate with the calculator variable name varnam
!!    ioflag    journal logging type passed on to journal(3f) procedure. If it
!!              is not present or blank, the journal(3f) routine is not evoked.
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_stuffa
!!    use M_calculator, only : stuffa
!!    use M_calculator, only : snum0
!!    implicit none
!!       call stuffa('$A','')
!!       call stuffa('$mystring','this is the value of the string')
!!       write(*,*)snum0('$mystring')
!!       call stuffa('$mystring','this is the new value of the string')
!!       write(*,*)snum0('$mystring')
!!    end program demo_stuffa
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine stuffa(varnam,string,ioflag)

! ident_15="@(#) M_calculator stuffa(3f) directly store a string into calculator variable name table"

character(len=*),intent(in)           :: varnam    !  assuming friendly, not checking for null or too long varnam0
character(len=:),allocatable          :: varnam_local
character(len=*),intent(in)           :: string
character(len=*),intent(in),optional  :: ioflag
character(len=:),allocatable          :: ioflag_local
integer                               :: indx
integer                               :: ierr
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(ioflag))then
      ioflag_local=trim(ioflag)
   else
      ioflag_local=' '
   endif
   varnam_local=adjustl(trim(varnam))
   ierr=0
!-----------------------------------------------------------------------------------------------------------------------------------
   call locate(keys_q,varnam_local,indx,ierr)
   if(indx.le.0)then                                        ! variable name not in dictionary
      indx=iabs(indx)
      call insert(keys_q,varnam_local,indx)                 ! adding the new variable name to the variable name array
      call insert(values,' '         ,indx)
      call insert(values_len,0       ,indx)
   elseif(ioflag_local.ne.'')then                           ! display variable string to trail and output as indicated by ioflag
      write(*,g)ioflag,varnam_local//'=',string
   endif
   ! found variable name in dictionary
   call replace(values,string,indx)
   call replace(values_len,len_trim(string),indx)
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine stuffa
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!      inum0(3f) - [M_calculator] return integer value from calculator expression
!!      (LICENSE:PD)
!!##SYNOPSIS
!!
!!   integer function inum0(inline,ierr)
!!
!!    character(len=*),intent(in)  :: inline
!!    integer,optional,intent(out) :: ierr
!!
!!##SYNOPSIS
!!
!!    INUM0() evaluates a CHARACTER argument as a FORTRAN-like
!!    calculator expression and returns an integer.
!!
!!     o INUM0() uses the calculator routine CALCULATOR(3f)
!!     o Remember that the calculator treats all values as DOUBLEPRECISION.
!!
!!    Values returned are assumed to be very close to being whole integer
!!    values. A small value (0.01) is added to the result before it is
!!    returned to reduce roundoff error problems. This could introduce
!!    errors if INUM0 is misused and is not being used to calculate
!!    essentially integer results.
!!##DESCRIPTION
!!
!!    inline  INLINE is a CHARACTER variable up to 255 characters long that is
!!            similar to a FORTRAN 77 numeric expression. Keep it less than 80
!!            characters when possible.
!!    ierr    zero (0) if no error occurs
!!
!!##DEPENDENCIES
!!         All programs that call the calculator routine can supply their
!!         own substitute_subroutine(3f) and substitute_C(3f) procedures. See
!!         the ../html/Example.html">example program for samples.
!!##EXAMPLES
!!
!!    Sample program:
!!
!!       program demo_inum0
!!       use M_calculator, only : inum0
!!       implicit none
!!       integer :: i,j,k
!!          i=inum0('20/3.4')
!!          j=inum0('CI = 13 * 3.1')
!!          k=inum0('CI')
!!          write(*,*)'Answers are ',I,J,K
!!       end program demo_inum0
!!
!!##SEE ALSO
!!       The syntax of an expression is as described in
!!       the main document of the Calculator Library.
!!   See
!!       CALCULATOR(),
!!       RNUM0(),
!!       DNUM0(),
!!       SNUM0(),
!!       EXPRESSION()
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!>
!! AUTHOR:  John S. Urban
!!##VERSION: 19971123
!-----------------------------------------------------------------------------------------------------------------------------------
integer function inum0(inline,ierr)

! ident_16="@(#) M_calculator inum0(3f) resolve a calculator string into a whole integer number"

!  The special string '*' returns -99999, otherwise return 0 on errors
character(len=*),intent(in)  :: inline
integer,optional,intent(out) :: ierr
!-----------------------------------------------------------------------------------------------------------------------------------
integer,parameter            :: IBIG=2147483647               ! overflow value (2**31-1)
integer                      :: iend
real,parameter               :: SMALL=0.0001                  ! and epsilon value
doubleprecision              :: dnum1
character(len=iclen_calc)    :: cdum20
integer                      :: ierr_local
integer                      :: ilen
!-----------------------------------------------------------------------------------------------------------------------------------
ierr_local=0
if(inline.eq.' ')then                                      ! return 0 for a blank string
   dnum1=0.0d0
elseif(inline.eq.'*')then                                  ! return -99999 on special string "*"
   dnum1=-99999d0
else                                                       ! parse string using calculator function
   iend=len(inline)
   call expression(inline(:iend),dnum1,cdum20,ierr_local,ilen)
   if(ierr_local.ne.0)then
      dnum1=0.0d0
   endif
endif
if(present(ierr))then
   ierr=ierr_local
endif
!-----------------------------------------------------------------------------------------------------------------------------------
! on most machines int() would catch the overflow, but this is safer
if(dnum1.gt.IBIG)then
   write(*,g)'*inum0* integer overflow 2**31-1 <',dnum1
   inum0=IBIG
elseif(dnum1.gt.0)then
   inum0=int(dnum1+SMALL)
else
   inum0=int(dnum1-SMALL)
endif
!-----------------------------------------------------------------------------------------------------------------------------------
end function inum0
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!       rnum0(3f) - [M_calculator] returns real number from string expression using CALCULATOR(3f)
!!       (LICENSE:PD)
!!##SYNOPSIS
!!
!!    real function rnum0(inline)
!!
!!     character(len=*), intent=(in) :: inline
!!     integer,intent(out),optional  :: ierr
!!
!!##DESCRIPTION
!!     RNUM0() is used to return a REAL value from a CHARACTER string representing
!!     a numeric expression. It uses the M_calculator(3fp) module.
!!
!!     inline  INLINE is a CHARACTER variable up to (iclen_calc=512) characters long
!!             that is similar to a FORTRAN 77 numeric expression.
!!     ierr    error code. If zero, no error occurred
!!
!!##DEPENDENCIES
!!       o User-supplied routines:
!!         All programs that call the calculator routine can supply their
!!         own substitute_subroutine(3f) and substitute_C(3f) procedures. See
!!         the example program for samples.
!!##EXAMPLES
!!
!!   Sample program
!!
!!     program demo_rnum0
!!     use M_calculator, only : rnum0
!!     implicit none
!!     real :: x, y, z
!!        x=rnum0('20/3.4')
!!        y=rnum0('CI = 10 * sin(3.1416/4)')
!!        z=rnum0('CI')
!!        write(*,*)x,y,z
!!     end program demo_rnum0
!!
!!##SEE ALSO
!!
!!       o The syntax of an expression is as described in the main documentation
!!         of the Calculator Library.
!!       o See EXPRESSION(3f), CALCULATOR(3f), INUM0(3f), DNUM0(3f), SNUM0(3f).
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!>
!! AUTHOR    John S. Urban
!!##VERSION   1.0,19971123
real function rnum0(inline,ierr)

! ident_17="@(#) M_calculator rnum0(3f) resolve a calculator string into a real number"

!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)  :: inline
integer,optional,intent(out) :: ierr
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=iclen_calc)    :: cdum20
doubleprecision              :: d_answer
integer                      :: ierr_local
integer                      :: ilen
integer                      :: iend
!-----------------------------------------------------------------------------------------------------------------------------------
   ierr_local=0
   if(inline.eq.' ')then
      d_answer=0.0d0
   elseif(inline.eq.'*')then                            !  the special string '*' returns -99999.0
      d_answer=-99999.0d0
   else
      iend=len(inline)
      call expression(inline(:iend),d_answer,cdum20,ierr_local,ilen)
      if(ierr_local.ne.0)then
         d_answer=0.0d0
      endif
   endif
   if(present(ierr))then
      ierr=ierr_local
   endif
   rnum0=real(d_answer)
!-----------------------------------------------------------------------------------------------------------------------------------
end function rnum0
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!      dnum0(3f) - [M_calculator] return double precision value from string expression using calculator(3f)
!!      (LICENSE:PD)
!!##SYNOPSIS
!!
!!   doubleprecision function dnum0(inline,ierr)
!!
!!    character(len=*),intent(in) :: inline
!!    integer,optional,intent(out) :: ierr
!!
!!##DESCRIPTION
!!     DNUM0() is used to return a DOUBLEPRECISION value from a CHARACTER string
!!     representing a numeric expression.
!!       o If an error occurs in evaluating the expression INUM() returns zero(0).
!!       o DNUM0() ultimately uses the calculator routine CALCULATOR(3f)
!!
!!      inline  INLINE is a CHARACTER variable up to (iclen_calc=255) characters long
!!              that is similar to a FORTRAN 77 numeric expression.
!!      ierr    error code. If zero, no error occurred
!!
!!##EXAMPLES
!!
!!   Sample Program
!!
!!     program demo_dnum0
!!     use M_calculator, only : dnum0
!!     implicit none
!!        doubleprecision x,y,z
!!        X=DNUM0('20/3.4')
!!        Y=DNUM0('CI = 10 * sin(3.1416/4)')
!!        Z=DNUM0('CI')
!!        write(*,*)x,y,z
!!     end program demo_dnum0
!!
!!##SEE ALSO
!!
!!       o The syntax of an expression is as described in the main documentation of the Calculator Library.
!!       o See EXPRESSION(), CALCULATOR(), RNUM0(), SNUM0().
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!>
!! AUTHOR + John S. Urban
!!##VERSION 1.0, 19971123
doubleprecision function dnum0(inline,ierr)

! ident_18="@(#) M_calculator dnum0(3f) resolve a calculator string into a doubleprecision number"

character(len=*),intent(in) :: inline
integer,optional,intent(out) :: ierr
character(len=iclen_calc)           :: cdum20
doubleprecision             :: dnum1
integer                     :: iend
integer                     :: ierr_local
integer                     :: ilen
   ierr_local=0
   if(inline.eq.' ')then
      dnum1=0.0d0
   elseif(inline.eq.'*')then    !  the special string '*' returns -99999.0
      dnum1=-99999.0d0
   else
      iend=len(inline)
      call expression(inline(:iend),dnum1,cdum20,ierr_local,ilen)
      if(ierr_local.ne.0)then
         dnum1=0.0d0
      endif
   endif
   dnum0=dnum1
   if(present(ierr))then
      ierr=ierr_local
   endif
end function dnum0
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!     snum0(3f) - [M_calculator] resolve a calculator expression into a string(return blank on errors)
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   function snum0(inline0,ierr)
!!
!!    character(len=:),allocatable :: snum0(inline0)
!!    character(len=*),intent(in)  :: inline0                           ! input string
!!    integer,optional,intent(out) :: ierr
!!
!!##DESCRIPTION
!!     SNUM0() is used to return a string value up to (iclen_calc=512) characters
!!     long from a string expression.
!!     SNUM0() uses the calculator routine CALCULATOR(3f)
!!
!!     inline0  INLINE0 is a CHARACTER variable up to (iclen_calc=512) characters long that
!!              is similar to a FORTRAN 77 expression.
!!     ierr     error code. If zero, no error occurred
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!     program demo_snum0
!!     use m_calculator, only: rnum0, snum0
!!     implicit none
!!     real :: rdum
!!     character(len=80)  :: ic,jc,kc
!!
!!        rdum=rnum0('A=83/2') ! set a variable in the calculator
!!        kc=snum0('$MYTITLE="This is my title variable"')
!!
!!        ic=snum0('$STR("VALUE IS [",A,"]")')
!!        jc=snum0('$MYTITLE')
!!
!!        write(*,*)'IC=',trim(ic)
!!        write(*,*)'JC=',trim(jc)
!!        write(*,*)'KC=',trim(kc)
!!
!!     end program demo_snum0
!!
!!    The output should look like
!!
!!      IC=VALUE IS [41.5]
!!      JC=This is my title variable
!!      KC=This is my title variable
!!
!!##DEPENDENCIES
!!       o User-supplied routines:
!!         All programs that call the calculator routine can supply their
!!         own substitute_subroutine(3f) and substitute_C(3f) procedures. See
!!         the example program for samples.
!!
!!##SEE ALSO
!!       o The syntax of an expression is described in the main document of the
!!         Calculator Library.
!!       o See CALCULATOR(), RNUM0(), SNUM0(), EXPRESSION().
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!>
!! AUTHOR    John S. Urban
!!##VERSION   1.0, 19971123
!===================================================================================================================================
function snum0(inline0,ierr)

! ident_19="@(#) M_calculator snum0(3f) resolve a calculator expression into a string"

!  a few odd things are done because some compilers did not work as expected
character(len=:),allocatable :: snum0
character(len=*),intent(in)  :: inline0                           ! input string
integer,optional,intent(out) :: ierr
character(len=iclen_calc)    :: lcopy                             ! working string
character(len=iclen_calc)    :: inline                            ! working copy of input string
integer                      :: ierr_local
integer                      :: iend                              ! size of input string
integer                      :: ilen
doubleprecision              :: dnum1

   inline=inline0                                                 ! some compilers need a copy of known length to work as expected
   ierr_local=0
   if(inline.eq.' ')then                                          ! what to do for a blank string
      snum0=' '                                                   ! return a blank string
   else                                                           ! non-blank input expression
      iend=len(inline)                                            ! size of working string
      lcopy=' '                                                   ! initialize trimmed string
      lcopy=adjustl(inline(:iend))                                ! trim leading spaces
      if(lcopy(1:1).eq.'$'.or.lcopy(1:1).eq.'"')then              ! this is a string that needs evaluated
         dnum1=0.0d0
         call expression(inline(:iend),dnum1,lcopy,ierr_local,ilen)
         if(ierr_local.ne.2)then                                  ! check if expression was evaluated to a string successfully
            snum0=' '                                             ! input string was not resolved to a string
         endif
         snum0=lcopy(:max(1,ilen))                                ! return whatever expression() returned
      else                                                        ! input was just a string, not an expression so just copy it
         snum0=inline(:iend)                                      ! copy input string to output
      endif
   endif
   if(present(ierr))then
      ierr=ierr_local
   endif
end function snum0
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!     expression(3f) - [M_calculator] return value from a string expression processing messages to simplify call to CALCULATOR(3f)
!!     (LICENSE:PD)
!!##SYNOPSIS
!!
!!    subroutine expression(inlin0,outval,outlin0,ierr,ilen)
!!
!!     character(len=*), intent=(in)  :: inlin0
!!     doubleprecision, intent=(out)  :: outval
!!     character(len=*), intent=(out) :: outlin0
!!     integer, intent=(out)          :: ierr
!!     integer, intent=(out)          :: ilen
!!
!!##DESCRIPTION
!!     expression() takes a string containing a FORTRAN-like expression and evaluates
!!     it and returns a numeric or string value as appropriate.
!!     The main purpose of expression() is to assume the burden of displaying the
!!     calculator messages for codes that make multiple calls to CALCULATOR(3f).
!!     CALCULATOR(3f) does not display error messages directly.
!!
!!       o EXPRESSION(3f) calls the calculator routine CALCULATOR(3f) to evaluate the
!!         expressions.
!!       o Messages beginning with a # are considered comments and are not passed
!!         on to the calculator.
!!
!!     inlin0  INLIN0 is a string containing a numeric expression. The expression can
!!             be up to (iclen_calc=512) characters long. The syntax of an expression
!!             is as described in the main document of the Calc library. For example:
!!
!!               'A=sin(3.1416/5)'
!!               '# this is a comment'
!!               '$STR("The value is ",40/3)'
!!
!!     outval  OUTVAL is a numeric value calculated from the expression in INLIN0
!!             (when IERR returns 0).
!!             When a string value is returned (IERR=2) then OUTVAL is the length of
!!             the output string.
!!     outlin0  OUTLIN0 contains a string representation of the number returned in
!!              OUTVAL up to 23 characters long when INLIN0 is a numeric expression. It
!!              contains a string up to (iclen_calc=512) characters long when INLIN0 is
!!              a string expression.
!!     ierr    IERR returns
!!
!!             o -1 if an error occurred
!!             o 0 if a numeric value is returned (value is in OUTVAL, string
!!               representation of the value is in OUTLIN2).
!!             o 1 if no value was returned but a message was displayed (If a 'dump'
!!               or 'funcs' command was passed to the calculator).
!!             o 2 if the expression evaluated to a string value instead of a
!!               numeric value (value is in OUTLIN0).
!!     ilen    ILEN returns the length of the input string minus trailing blanks.
!!
!!##DEPENDENCIES
!!       o User-supplied routines:
!!         All programs that call the calculator routine can supply their
!!         own substitute_subroutine(3f) and substitute_C(3f) procedures. See
!!         the example program for samples.
!!##EXAMPLES
!!
!!    Sample program:
!!
!!     program demo_expression
!!     use M_calculator, only : iclen_calc
!!     use M_calculator, only : expression
!!     implicit none
!!     character(len=iclen_calc) ::  outlin0
!!     doubleprecision :: outval
!!     integer :: ierr, ilen
!!        call expression('A=3.4**5    ',outval,outlin0,ierr,ilen)
!!        write(*,*)'value of expression is ',outval
!!        write(*,*)'string representation of value is ',trim(outlin0)
!!        write(*,*)'error flag value is ',ierr
!!        write(*,*)'length of expression is ',ilen
!!     end program demo_expression
!!
!!   Results:
!!
!!     value of expression is    454.35424000000000
!!     string representation of value is 454.35424
!!     error flag value is            0
!!     length of expression is            8
!!
!!##SEE ALSO
!!     See also: RNUM0(),CALCULATOR(),INUM0(),SNUM0()
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!>
!! AUTHOR   John S. Urban
!!##VERSION  V1.0, 19971123
recursive subroutine expression(inlin0,outval,outlin0,ierr,ilen)

! ident_20="@(#) M_calculator expression(3f) call CALCULATOR(3f) calculator and display messages"

! evaluate a FORTRAN-like string expression and return a numeric
! value and its character equivalent or a string value as appropriate
character(len=*),intent(in) :: inlin0
doubleprecision             :: outval
character(len=*)            :: outlin0
integer,intent(out)         :: ierr
integer,intent(out)         :: ilen

character(len=iclen_calc)   :: line
character(len=iclen_calc)   :: outlin
doubleprecision,save        :: rvalue=0.0d0
intrinsic                   :: len
integer                     :: imaxi
character(len=iclen_calc)   :: event
!#----------------------------------------------------------------------------------------------------------------------------------
   ! copy INLIN0 to working copy LINE and find position of last non-blank character
   ! in the string
   line=''
   line=inlin0
   ! if the line is blank set imaxi to 1, else set it to the least of the length of the input string or (iclen_calc)
   ! NOTE: not checking if input expression is longer than (iclen_calc) characters!!
   imaxi=max(min(len(line),len(inlin0)),1)
   ilen=len_trim(line(1:imaxi))
!-----------------------------------------------------------------------------------------------------------------------------------
   if(ilen.eq.0)then                                            ! command was totally blank
      ierr=-1
      write(*,g)'*expression* warning===> blank expression'
!-----------------------------------------------------------------------------------------------------------------------------------
   elseif(line(:1).eq.'#')then                                  ! line was a comment
!-----------------------------------------------------------------------------------------------------------------------------------
   else
      ierr=0
      call calculator(line(:ilen),outlin,event,rvalue,ierr)         ! evaluate the expression
!-----------------------------------------------------------------------------------------------------------------------------------
      select case(ierr)
      case(-1)                                    ! trapped error, display error message
        write(*,g)'*expression* error===>',event
        !call pdec(line(:ilen))                   ! echo input string as is and in ASCII decimal
      case(1)                                     ! general message, display message
        write(*,g)'*expression* message===>',event
      case(0)                                     ! numeric output
         outlin0=outlin
      case(2)                                     ! string output
         outlin0=event                            ! assumes outlin is long enough to return the string into
         ilen=int(rvalue)                         ! in special mode where a string is returned, rvalue is the length of the string
      case default
        write(*,g)'*expression* warning===> unexpected ierr value=',ierr
      end select
!-----------------------------------------------------------------------------------------------------------------------------------
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   outval=rvalue                            ! return normal sized real value
end subroutine expression
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine juown1_placeholder(func,iflen,args,iargstp,n,fval,ctmp,ier)
      ! extend functions available to the calculator routine
!
!     if the function ownmode(1) is called this subroutine
!     will be accessed to do user-written functions.
!
!     func(iend-1)=procedure name.  func should not be changed.
!     iflen=length of procedure name.
!     args=array of 100 elements containing procedure arguments.
!     iargstp=type of argument(1=value,2=position of string value)
!     n=integer number of parameters
!     x=array of 55555 x values
!     y=array of 55555 y values
!     fval=value to replace function call
!     ctmp=string to return when returning a string value
!     ier=returned error flag value.
!         set to -1 if an error occurs.
!         set to  0 if a number is returned
!         set to  2 if a string is returned
!
!!use M_calculator, only : x, y, values, values_len
character(len=*),intent(in) :: func
integer,intent(in)          :: iflen
real(kind=db),intent(in)    :: args(100)
integer,intent(in)          :: iargstp(100)
integer,intent(in)          :: n
real(kind=db)               :: fval
character(len=*)            :: ctmp
integer                     :: ier

integer                     :: i10
integer                     :: iwhich
integer                     :: ilen
!-----------------------------------------------------------------------
   fval=0.0d0
!-----------------------------------------------------------------------
   write(*,*)'*juown1_placeholder* unknown function ', func(1:iflen)
   write(*,*)'function name length is..',iflen
   write(*,*)'number of arguments .....',n
   do i10=1,n
      if(iargstp(i10).eq.0)then
         write(*,*)i10,' VALUE=',args(i10)
      elseif(iargstp(i10).eq.2)then
         iwhich=int(args(i10)+0.5d0)
         ilen=values_len(iwhich)
         write(*,*)i10,' STRING='//values(iwhich)(:ilen)
      else
         write(*,*)'unknown parameter type is ',iargstp(i10)
      endif
   enddo
end subroutine juown1_placeholder
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
real function c_placeholder(args,n)
! a built-in calculator function called c must be satisfied.
! write whatever you want here as a function
integer,intent(in)          :: n
real(kind=db),intent(in) :: args(n)
   c_placeholder=0.0_db
end function c_placeholder
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!use M_strings, only : upper, lower, value_to_string
!use M_list,    only : locate, insert, replace
!===================================================================================================================================
elemental pure function upper(str,begin,end) result (string)

!character(len=*),parameter::ident_21="@(#)M_strings::upper(3f): Changes a string to uppercase"

character(*), intent(In)      :: str                 ! inpout string to convert to all uppercase
integer, intent(in), optional :: begin,end
character(len(str))           :: string              ! output string that contains no miniscule letters
integer                       :: i                   ! loop counter
integer                       :: ibegin,iend
   string = str                                      ! initialize output string to input string

   ibegin = 1
   if (present(begin))then
      ibegin = max(ibegin,begin)
   endif

   iend = len_trim(str)
   if (present(end))then
      iend= min(iend,end)
   endif

   do i = ibegin, iend                               ! step thru each letter in the string in specified range
       select case (str(i:i))
       case ('a':'z')                                ! located miniscule letter
          string(i:i) = char(iachar(str(i:i))-32)    ! change miniscule letter to uppercase
       end select
   end do

end function upper
!===================================================================================================================================
elemental pure function lower(str,begin,end) result (string)

!character(len=*),parameter::ident_22="@(#)M_strings::lower(3f): Changes a string to lowercase over specified range"

character(*), intent(In)     :: str
character(len(str))          :: string
integer,intent(in),optional  :: begin, end
integer                      :: i
integer                      :: ibegin, iend
   string = str

   ibegin = 1
   if (present(begin))then
      ibegin = max(ibegin,begin)
   endif

   iend = len_trim(str)
   if (present(end))then
      iend= min(iend,end)
   endif

   do i = ibegin, iend                               ! step thru each letter in the string in specified range
      select case (str(i:i))
      case ('A':'Z')
         string(i:i) = char(iachar(str(i:i))+32)     ! change letter to miniscule
      case default
      end select
   end do

end function lower
!===================================================================================================================================
subroutine delim(line,array,n,icount,ibegin,iterm,ilen,dlim)

!character(len=*),parameter::ident_9="@(#)M_strings::delim(3f): parse a string and store tokens into an array"

!
!     given a line of structure " par1 par2 par3 ... parn "
!     store each par(n) into a separate variable in array.
!
!     IF ARRAY(1) == '#N#' do not store into string array  (KLUDGE))
!
!     also count number of elements of array initialized, and
!     return beginning and ending positions for each element.
!     also return position of last non-blank character (even if more
!     than n elements were found).
!
!     no quoting of delimiter is allowed
!     no checking for more than n parameters, if any more they are ignored
!
character(len=*),intent(in)    :: line
integer,intent(in)             :: n
character(len=*)               :: array(n)
integer,intent(out)            :: icount
integer,intent(out)            :: ibegin(n)
integer,intent(out)            :: iterm(n)
integer,intent(out)            :: ilen
character(len=*),intent(in)    :: dlim
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=len(line)):: line_local
logical             :: lstore
integer             :: i10
integer             :: iarray
integer             :: icol
integer             :: idlim
integer             :: iend
integer             :: ifound
integer             :: istart
!-----------------------------------------------------------------------------------------------------------------------------------
      icount=0
      ilen=len_trim(line)
      line_local=line

      idlim=len(dlim)
      if(idlim > 5)then
         idlim=len_trim(dlim)      ! dlim a lot of blanks on some machines if dlim is a big string
         if(idlim == 0)then
            idlim=1     ! blank string
         endif
      endif

      if(ilen == 0)then                                        ! command was totally blank
         return
      endif
!
!     there is at least one non-blank character in the command
!     ilen is the column position of the last non-blank character
!     find next non-delimiter
      icol=1

      if(array(1) == '#N#')then                                ! special flag to not store into character array
         lstore=.false.
      else
         lstore=.true.
      endif

      do iarray=1,n,1                                          ! store into each array element until done or too many words
         NOINCREMENT: do
            if(index(dlim(1:idlim),line_local(icol:icol)) == 0)then  ! if current character is not a delimiter
               istart=icol                                     ! start new token on the non-delimiter character
               ibegin(iarray)=icol
               iend=ilen-istart+1+1                            ! assume no delimiters so put past end of line
               do i10=1,idlim
                  ifound=index(line_local(istart:ilen),dlim(i10:i10))
                  if(ifound > 0)then
                     iend=min(iend,ifound)
                  endif
               enddo
               if(iend <= 0)then                               ! no remaining delimiters
                 iterm(iarray)=ilen
                 if(lstore)then
                    array(iarray)=line_local(istart:ilen)
                 endif
                 icount=iarray
                 return
               else
                 iend=iend+istart-2
                 iterm(iarray)=iend
                 if(lstore)then
                    array(iarray)=line_local(istart:iend)
                 endif
               endif
               icol=iend+2
               exit NOINCREMENT
            endif
            icol=icol+1
         enddo NOINCREMENT
!        last character in line was a delimiter, so no text left
!        (should not happen where blank=delimiter)
         if(icol > ilen)then
           icount=iarray
           if( (iterm(icount)-ibegin(icount)) < 0)then         ! last token was all delimiters
              icount=icount-1
           endif
           return
         endif
      enddo
      icount=n  ! more than n elements
end subroutine delim
!===================================================================================================================================
subroutine value_to_string(gval,chars,length,err,fmt,trimz)

!character(len=*),parameter::ident_40="@(#)M_strings::value_to_string(3fp): subroutine returns a string from a value"

class(*),intent(in)                      :: gval
character(len=*),intent(out)             :: chars
integer,intent(out),optional             :: length
integer,optional                         :: err
integer                                  :: err_local
character(len=*),optional,intent(in)     :: fmt         ! format to write value with
logical,intent(in),optional              :: trimz
character(len=:),allocatable             :: fmt_local
character(len=1024)                      :: msg

!  Notice that the value GVAL can be any of several types ( INTEGER,REAL,DOUBLEPRECISION,LOGICAL)

   if (present(fmt)) then
      select type(gval)
      type is (integer)
         fmt_local='(i0)'
         if(fmt.ne.'') fmt_local=fmt
         write(chars,fmt_local,iostat=err_local,iomsg=msg)gval
      type is (real)
         fmt_local='(bz,g23.10e3)'
         fmt_local='(bz,g0.8)'
         if(fmt.ne.'') fmt_local=fmt
         write(chars,fmt_local,iostat=err_local,iomsg=msg)gval
      type is (doubleprecision)
         fmt_local='(bz,g0)'
         if(fmt.ne.'') fmt_local=fmt
         write(chars,fmt_local,iostat=err_local,iomsg=msg)gval
      type is (logical)
         fmt_local='(l1)'
         if(fmt.ne.'') fmt_local=fmt
         write(chars,fmt_local,iostat=err_local,iomsg=msg)gval
      class default
         write(*,*)'*value_to_string* UNKNOWN TYPE'
         chars=' '
      end select
      if(fmt.eq.'') then
         chars=adjustl(chars)
         call trimzeros(chars)
      endif
   else                                                  ! no explicit format option present
      err_local=-1
      select type(gval)
      type is (integer)
         write(chars,*,iostat=err_local,iomsg=msg)gval
      type is (real)
         write(chars,*,iostat=err_local,iomsg=msg)gval
      type is (doubleprecision)
         write(chars,*,iostat=err_local,iomsg=msg)gval
      type is (logical)
         write(chars,*,iostat=err_local,iomsg=msg)gval
      class default
         chars=''
      end select
      chars=adjustl(chars)
      if(index(chars,'.').ne.0) call trimzeros(chars)
   endif
   if(present(trimz))then
      if(trimz)then
         chars=adjustl(chars)
         call trimzeros(chars)
      endif
   endif

   if(present(length)) then
      length=len_trim(chars)
   endif

   if(present(err)) then
      err=err_local
   elseif(err_local.ne.0)then
      !! cannot currently do I/O from a function being called from I/O
      !!write(ERROR_UNIT,'(a)')'*value_to_string* WARNING:['//trim(msg)//']'
      chars=chars//' *value_to_string* WARNING:['//trim(msg)//']'
   endif

end subroutine value_to_string
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine locate_c(list,value,place,ier,errmsg)

!character(len=*),parameter::ident_5="&
!&@(#)M_list::locate_c(3f): find PLACE in sorted character array where VALUE can be found or should be placed"

character(len=*),intent(in)             :: value
integer,intent(out)                     :: place
character(len=:),allocatable            :: list(:)
integer,intent(out),optional            :: ier
character(len=*),intent(out),optional   :: errmsg
integer                                 :: i
character(len=:),allocatable            :: message
integer                                 :: arraysize
integer                                 :: maxtry
integer                                 :: imin, imax
integer                                 :: error
   if(.not.allocated(list))then
      list=[character(len=max(len_trim(value),2)) :: ]
   endif
   arraysize=size(list)

   error=0
   if(arraysize.eq.0)then
      maxtry=0
      place=-1
   else
      maxtry=int(log(float(arraysize))/log(2.0)+1.0)
      place=(arraysize+1)/2
   endif
   imin=1
   imax=arraysize
   message=''

   LOOP: block
   do i=1,maxtry

      if(value.eq.list(PLACE))then
         exit LOOP
      else if(value.gt.list(place))then
         imax=place-1
      else
         imin=place+1
      endif

      if(imin.gt.imax)then
         place=-imin
         if(iabs(place).gt.arraysize)then ! ran off end of list. Where new value should go or an unsorted input array'
            exit LOOP
         endif
         exit LOOP
      endif

      place=(imax+imin)/2

      if(place.gt.arraysize.or.place.le.0)then
         message='*locate* error: search is out of bounds of list. Probably an unsorted input array'
         error=-1
         exit LOOP
      endif

   enddo
   message='*locate* exceeded allowed tries. Probably an unsorted input array'
   endblock LOOP
   if(present(ier))then
      ier=error
   else if(error.ne.0)then
      write(stderr,*)message//' VALUE=',trim(value)//' PLACE=',place
      stop 1
   endif
   if(present(errmsg))then
      errmsg=message
   endif
end subroutine locate_c
!===================================================================================================================================
subroutine locate_d(list,value,place,ier,errmsg)

!character(len=*),parameter::ident_6="&
!&@(#)M_list::locate_d(3f): find PLACE in sorted doubleprecision array where VALUE can be found or should be placed"

! Assuming an array sorted in descending order
!
!  1. If it is not found report where it should be placed as a NEGATIVE index number.

doubleprecision,allocatable            :: list(:)
doubleprecision,intent(in)             :: value
integer,intent(out)                    :: place
integer,intent(out),optional           :: ier
character(len=*),intent(out),optional  :: errmsg

integer                                :: i
character(len=:),allocatable           :: message
integer                                :: arraysize
integer                                :: maxtry
integer                                :: imin, imax
integer                                :: error

   if(.not.allocated(list))then
      list=[doubleprecision :: ]
   endif
   arraysize=size(list)

   error=0
   if(arraysize.eq.0)then
      maxtry=0
      place=-1
   else
      maxtry=int(log(float(arraysize))/log(2.0)+1.0)
      place=(arraysize+1)/2
   endif
   imin=1
   imax=arraysize
   message=''

   LOOP: block
   do i=1,maxtry

      if(value.eq.list(PLACE))then
         exit LOOP
      else if(value.gt.list(place))then
         imax=place-1
      else
         imin=place+1
      endif

      if(imin.gt.imax)then
         place=-imin
         if(iabs(place).gt.arraysize)then ! ran off end of list. Where new value should go or an unsorted input array'
            exit LOOP
         endif
         exit LOOP
      endif

      place=(imax+imin)/2

      if(place.gt.arraysize.or.place.le.0)then
         message='*locate* error: search is out of bounds of list. Probably an unsorted input array'
         error=-1
         exit LOOP
      endif

   enddo
   message='*locate* exceeded allowed tries. Probably an unsorted input array'
   endblock LOOP
   if(present(ier))then
      ier=error
   else if(error.ne.0)then
      write(stderr,*)message//' VALUE=',value,' PLACE=',place
      stop 1
   endif
   if(present(errmsg))then
      errmsg=message
   endif
end subroutine locate_d
!===================================================================================================================================
subroutine remove_i(list,place)

!character(len=*),parameter::ident_13="@(#)M_list::remove_i(3fp): remove value from allocatable array at specified position"
integer,allocatable    :: list(:)
integer,intent(in)     :: place
integer                :: end

   if(.not.allocated(list))then
      list=[integer :: ]
   endif
   end=size(list)
   if(place.le.0.or.place.gt.end)then                       ! index out of bounds of array
   elseif(place.eq.end)then                                 ! remove from array
      list=[ list(:place-1)]
   else
      list=[ list(:place-1), list(place+1:) ]
   endif

end subroutine remove_i
!===================================================================================================================================
subroutine remove_c(list,place)

!character(len=*),parameter::ident_9="@(#)M_list::remove_c(3fp): remove string from allocatable string array at specified position"

character(len=:),allocatable :: list(:)
integer,intent(in)           :: place
integer                      :: ii, end
   if(.not.allocated(list))then
      list=[character(len=2) :: ]
   endif
   ii=len(list)
   end=size(list)
   if(place.le.0.or.place.gt.end)then                       ! index out of bounds of array
   elseif(place.eq.end)then                                 ! remove from array
      list=[character(len=ii) :: list(:place-1) ]
   else
      list=[character(len=ii) :: list(:place-1), list(place+1:) ]
   endif
end subroutine remove_c
subroutine remove_d(list,place)

!character(len=*),parameter::ident_10="&
!&@(#)M_list::remove_d(3fp): remove doubleprecision value from allocatable array at specified position"

doubleprecision,allocatable  :: list(:)
integer,intent(in)           :: place
integer                      :: end
   if(.not.allocated(list))then
           list=[doubleprecision :: ]
   endif
   end=size(list)
   if(place.le.0.or.place.gt.end)then                       ! index out of bounds of array
   elseif(place.eq.end)then                                 ! remove from array
      list=[ list(:place-1)]
   else
      list=[ list(:place-1), list(place+1:) ]
   endif

end subroutine remove_d
!===================================================================================================================================
subroutine replace_c(list,value,place)

!character(len=*),parameter::ident_14="@(#)M_list::replace_c(3fp): replace string in allocatable string array at specified position"

character(len=*),intent(in)  :: value
character(len=:),allocatable :: list(:)
character(len=:),allocatable :: kludge(:)
integer,intent(in)           :: place
integer                      :: ii
integer                      :: tlen
integer                      :: end
   if(.not.allocated(list))then
      list=[character(len=max(len_trim(value),2)) :: ]
   endif
   tlen=len(value)
   end=size(list)
   if(place.lt.0.or.place.gt.end)then
           write(stderr,*)'*replace_c* error: index out of range. end=',end,' index=',place
   elseif(len(value).le.len(list))then
      list(place)=value
   else  ! increase length of variable
      ii=max(tlen,len(list))
      kludge=[character(len=ii) :: list ]
      list=kludge
      list(place)=value
   endif
end subroutine replace_c
!===================================================================================================================================
subroutine replace_d(list,value,place)

!character(len=*),parameter::ident_15="&
!&@(#)M_list::replace_d(3fp): place doubleprecision value into allocatable array at specified position"

doubleprecision,intent(in)   :: value
doubleprecision,allocatable  :: list(:)
integer,intent(in)           :: place
integer                      :: end
   if(.not.allocated(list))then
           list=[doubleprecision :: ]
   endif
   end=size(list)
   if(end.eq.0)then                                          ! empty array
      list=[value]
   elseif(place.gt.0.and.place.le.end)then
      list(place)=value
   else                                                      ! put in middle of array
      write(stderr,*)'*replace_d* error: index out of range. end=',end,' index=',place
   endif
end subroutine replace_d
!===================================================================================================================================
subroutine replace_i(list,value,place)

!character(len=*),parameter::ident_18="@(#)M_list::replace_i(3fp): place value into allocatable array at specified position"

integer,intent(in)    :: value
integer,allocatable   :: list(:)
integer,intent(in)    :: place
integer               :: end
   if(.not.allocated(list))then
      list=[integer :: ]
   endif
   end=size(list)
   if(end.eq.0)then                                          ! empty array
      list=[value]
   elseif(place.gt.0.and.place.le.end)then
      list(place)=value
   else                                                      ! put in middle of array
      write(stderr,*)'*replace_i* error: index out of range. end=',end,' index=',place
   endif
end subroutine replace_i
!===================================================================================================================================
subroutine insert_c(list,value,place)

!character(len=*),parameter::ident_19="@(#)M_list::insert_c(3fp): place string into allocatable string array at specified position"

character(len=*),intent(in)  :: value
character(len=:),allocatable :: list(:)
character(len=:),allocatable :: kludge(:)
integer,intent(in)           :: place
integer                      :: ii
integer                      :: end

   if(.not.allocated(list))then
      list=[character(len=max(len(value),2)) :: ]
   endif

   ii=max(len(value),len(list),2)
   end=size(list)

   if(end.eq.0)then                                          ! empty array
      list=[character(len=ii) :: value ]
   elseif(place.eq.1)then                                    ! put in front of array
      kludge=[character(len=ii) :: value, list]
      list=kludge
   elseif(place.gt.end)then                                  ! put at end of array
      kludge=[character(len=ii) :: list, value ]
      list=kludge
   elseif(place.ge.2.and.place.le.end)then                 ! put in middle of array
      kludge=[character(len=ii) :: list(:place-1), value,list(place:) ]
      list=kludge
   else                                                      ! index out of range
      write(stderr,*)'*insert_c* error: index out of range. end=',end,' index=',place,' value=',value
   endif

end subroutine insert_c
!===================================================================================================================================
subroutine insert_d(list,value,place)

!character(len=*),parameter::ident_21="&
!&@(#)M_list::insert_d(3fp): place doubleprecision value into allocatable array at specified position"

doubleprecision,intent(in)       :: value
doubleprecision,allocatable      :: list(:)
integer,intent(in)               :: place
integer                          :: end
   if(.not.allocated(list))then
      list=[doubleprecision :: ]
   endif
   end=size(list)
   if(end.eq.0)then                                          ! empty array
      list=[value]
   elseif(place.eq.1)then                                    ! put in front of array
      list=[value, list]
   elseif(place.gt.end)then                                  ! put at end of array
      list=[list, value ]
   elseif(place.ge.2.and.place.le.end)then                 ! put in middle of array
      list=[list(:place-1), value,list(place:) ]
   else                                                      ! index out of range
      write(stderr,*)'*insert_d* error: index out of range. end=',end,' index=',place,' value=',value
   endif
end subroutine insert_d
!===================================================================================================================================
subroutine insert_i(list,value,place)

!character(len=*),parameter::ident_23="@(#)M_list::insert_i(3fp): place value into allocatable array at specified position"

integer,allocatable   :: list(:)
integer,intent(in)    :: value
integer,intent(in)    :: place
integer               :: end
   if(.not.allocated(list))then
      list=[integer :: ]
   endif
   end=size(list)
   if(end.eq.0)then                                          ! empty array
      list=[value]
   elseif(place.eq.1)then                                    ! put in front of array
      list=[value, list]
   elseif(place.gt.end)then                                  ! put at end of array
      list=[list, value ]
   elseif(place.ge.2.and.place.le.end)then                 ! put in middle of array
      list=[list(:place-1), value,list(place:) ]
   else                                                      ! index out of range
      write(stderr,*)'*insert_i* error: index out of range. end=',end,' index=',place,' value=',value
   endif

end subroutine insert_i
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function round(val,idigits0)
implicit none
!character(len=*),parameter :: ident="@(#) M_math::round(3f): round val to specified number of significant digits"
integer,parameter          :: dp=kind(0.0d0)
real(kind=dp),intent(in)   :: val
integer,intent(in)         :: idigits0
   integer                 :: idigits,ipow
   real(kind=dp)           :: aval,rnormal
   real(kind=dp)           :: round
!  this does not work very well because of round-off errors.
!  Make a better one, probably have to use machine-dependent bit shifting
   ! make sure a reasonable number of digits has been requested
   idigits=max(1,idigits0)
   aval=abs(val)
!  select a power that will normalize the number
!  (put it in the range 1 > abs(val) <= 0)
   if(aval.ge.1)then
      ipow=int(log10(aval)+1)
   else
      ipow=int(log10(aval))
   endif
   rnormal=val/(10.0d0**ipow)
   if(rnormal.eq.1)then
      ipow=ipow+1
   endif
   !normalize, multiply by 10*idigits to an integer, and so on
   round=real(anint(val*10.d0**(idigits-ipow)))*10.d0**(ipow-idigits)
end function round
!===================================================================================================================================
subroutine trimzeros(string)

!character(len=*),parameter::ident_50="@(#)M_strings::trimzeros(3fp): Delete trailing zeros from numeric decimal string"

! if zero needs added at end assumes input string has room
character(len=*)             :: string
character(len=len(string)+2) :: str
character(len=len(string))   :: exp          ! the exponent string if present
integer                      :: ipos         ! where exponent letter appears if present
integer                      :: i, ii
   str=string                                ! working copy of string
   ipos=scan(str,'eEdD')                     ! find end of real number if string uses exponent notation
   if(ipos>0) then                           ! letter was found
      exp=str(ipos:)                         ! keep exponent string so it can be added back as a suffix
      str=str(1:ipos-1)                      ! just the real part, exponent removed will not have trailing zeros removed
   endif
   if(index(str,'.').eq.0)then               ! if no decimal character in original string add one to end of string
      ii=len_trim(str)
      str(ii+1:ii+1)='.'                     ! add decimal to end of string
   endif
   do i=len_trim(str),1,-1                   ! scanning from end find a non-zero character
      select case(str(i:i))
      case('0')                              ! found a trailing zero so keep trimming
         cycle
      case('.')                              ! found a decimal character at end of remaining string
         if(i.le.1)then
            str='0'
         else
            str=str(1:i-1)
         endif
         exit
      case default
         str=str(1:i)                        ! found a non-zero character so trim string and exit
         exit
      end select
   end do
   if(ipos>0)then                            ! if originally had an exponent place it back on
      string=trim(str)//trim(exp)
   else
      string=str
   endif
end subroutine trimzeros
!===================================================================================================================================
subroutine init_random_seed(mine)

!character(len=*),parameter::ident_7="&
!&@(#)M_random::init_random_seed(3f): initialize random_number(3f) to return a single value with single integer seed like srand(3c)"

! to make this start with a single number like srand(3c) take the seed and
! use the value to fill the seed array, adding 37 to each subsequent value
! till the array is filled.
integer,intent(in) :: mine
   integer         :: i, n
   integer, dimension(:), allocatable :: seed
   call random_seed(size = n)
   allocate(seed(n))
   seed = mine + 37 * (/ (i - 1, i = 1, n) /)
  !write(*,*)seed
  !write(*,*)(/ (i - 1, i = 1, n) /)
   call random_seed(put = seed)
   deallocate(seed)
end subroutine init_random_seed
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_calculator
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
