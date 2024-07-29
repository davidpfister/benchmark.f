!
! CODE DEVELOPED BY SCOTT E BOYCE 
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
!
! NOTE THAT BLOCK CONSTRUCT IS USED BY SET_NAN FUNCTIONS
! IF COMPILER DOES NOT SUPPORT THIS FORTRAN 2008 FEATURE
! THEN REMOVE FUNCTIONS AND REPLACE WITH COMMENTED VERSIONS AT BOTTOM
! OF THIS SOURCE FILE.
!
!    **Note that a Fortran Gotcha is setting a float parameter without the decimal point 
!        results in an integer conversion before setting the parameter.
!        For example:
!                    REAL(DBL), PARAMETER:: DNEG = -1_dbl
!        really does:
!                    REAL(DBL), PARAMETER:: DNEG = REAL(-1_int64, dbl)
!        the correct way:
!                    REAL(DBL), PARAMETER:: DNEG = -1.0_dbl
!
MODULE EquationParser_CONSTANTS !, ONLY:BLNK,NL,TAB,COM,Z,ONE,TWO,THREE,FOUR,FIVE,TEN,DZ,UNO,DOS,DIEZ,NEG,DNEG,TRUE,FALSE,NEARZERO_5,NEARZERO_10,NEARZERO_30,NEARZERO_50, NEGNEARZERO_10, GET_NAN, SET_NaN
  !
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: i8 => INT8,   i16 => INT16,  &
                                         i32 => INT32,  i64 => INT64,  &
                                         SNG => REAL32, DBL => REAL64, &
                                                        QAD => REAL128
  !
  PRIVATE:: i8, i16, i32, i64, SNG, DBL, QAD
  !
  PRIVATE:: SET_REAL_NAN, SET_DBLE_NAN
  !
  !---------------------------------------------------------------------------------------------------
  !
  LOGICAL, PARAMETER:: TRUE       = .TRUE.
  LOGICAL, PARAMETER:: FALSE      = .FALSE.
  !        
  LOGICAL, PARAMETER::    Big_Endian = IACHAR( TRANSFER( 1_i16, ' ' ) ) == 0 ! Transfer b'00000000 00000001' to CHAR(1), which will drop either then 1st or 2nd byte. If 1st byte is kept, then CPU is Big-Endian
  LOGICAL, PARAMETER:: Little_Endian = .NOT. Big_Endian                      !   Note Itel/AMD x86 CPUs are Little-Endian
  !
  !---------------------------------------------------------------------------------------------------
  !
  !CHARACTER(0),  PARAMETER:: NULL_CHAR  = '' ! --> Not standard fortran to have zero length char
  !
  CHARACTER,     PARAMETER:: NL         = NEW_LINE(' ')
  CHARACTER(2),  PARAMETER:: BLN        = NL//NL
  !
  CHARACTER,     PARAMETER:: BLNK       = ' '
  CHARACTER(2),  PARAMETER:: BLNK2      = BLNK
  CHARACTER(3),  PARAMETER:: BLNK3      = BLNK
  CHARACTER(4),  PARAMETER:: BLNK4      = BLNK
  CHARACTER(5),  PARAMETER:: BLNK5      = BLNK
  !
  !---------------------------------------------------------------------------------------------------
  !
  CHARACTER,     PARAMETER:: TAB        =  ACHAR(9)
  CHARACTER,     PARAMETER:: COM        = '#'
  CHARACTER,     PARAMETER:: CM         = ','
  CHARACTER,     PARAMETER:: SLASH      = '/'
  CHARACTER,     PARAMETER:: BSLASH     = '\'
  CHARACTER,     PARAMETER:: MINUS      = '-'
  CHARACTER,     PARAMETER:: PLUS       = '+'
  CHARACTER,     PARAMETER:: EQ         = '='
  CHARACTER,     PARAMETER:: SQUOTE     = "'"
  CHARACTER,     PARAMETER:: DQUOTE     = '"'
  CHARACTER(2),  PARAMETER:: DEGREE_SIGN= 'Â°'   !Unicode 2btye representation of ° - Note this will not work for cmd.exe cuz it only supports ASCII
  !                                     
  CHARACTER(3),  PARAMETER:: YES        = 'YES'
  CHARACTER(2),  PARAMETER:: NO         = 'NO'
  !                                     
  CHARACTER,     PARAMETER:: CR         = ACHAR(13)  !CARRIAGE RETURN (UNIX ENDING)
  CHARACTER,     PARAMETER:: LF         = ACHAR(10)  !LINE FEED (CRLF IS WINDOWNS ENDING)
  CHARACTER(2),  PARAMETER:: winNL      = CR//LF
  !                                     
  CHARACTER(*), PARAMETER:: NUMBERS    ='0123456789'
  CHARACTER(*), PARAMETER:: NUMPNT     ='0123456789.'
  !
  CHARACTER(*), PARAMETER:: lowerCHAR="abcdefghijklmnopqrstuvwxyz"
  CHARACTER(*), PARAMETER:: upperCHAR="ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  !
  !---------------------------------------------------------------------------------------------------
  !
  INTEGER(i32),  PARAMETER:: NINER      = -999
  INTEGER(i32),  PARAMETER:: NEG3       = -3
  INTEGER(i32),  PARAMETER:: NEG2       = -2
  INTEGER(i32),  PARAMETER:: NEG        = -1
  INTEGER(i32),  PARAMETER:: Z          = 0
  INTEGER(i32),  PARAMETER:: ONE        = 1
  INTEGER(i32),  PARAMETER:: TWO        = 2
  INTEGER(i32),  PARAMETER:: THREE      = 3
  INTEGER(i32),  PARAMETER:: FOUR       = 4
  INTEGER(i32),  PARAMETER:: FIVE       = 5
  INTEGER(i32),  PARAMETER:: SIX        = 6
  INTEGER(i32),  PARAMETER:: SEV        = 7
  INTEGER(i32),  PARAMETER:: EIGHT      = 8
  INTEGER(i32),  PARAMETER:: NINE       = 9
  INTEGER(i32),  PARAMETER:: TEN        = 10
  INTEGER(i32),  PARAMETER:: TWENTY     = 20
  INTEGER(i32),  PARAMETER:: FIFTY      = 50
  INTEGER(i32),  PARAMETER:: SIXTY      = 60
  INTEGER(i32),  PARAMETER:: HUND       = 100
  INTEGER(i32),  PARAMETER:: QUIN       = 500
  INTEGER(i32),  PARAMETER:: THOU       = 1000
  INTEGER(i32),  PARAMETER:: THOU10     = 10000
  INTEGER(i32),  PARAMETER:: THOU20     = 20000
  INTEGER(i32),  PARAMETER:: THOU30     = 30000
  INTEGER(i32),  PARAMETER:: THOU40     = 30000
  INTEGER(i32),  PARAMETER:: THOU50     = 50000
  INTEGER(i32),  PARAMETER:: THOU60     = 60000
  !
  INTEGER(i32),  PARAMETER:: Upper_to_Lower = ICHAR('a')-ICHAR('A')  ! =  32 for ASCII shift -> Add to upcase to make lower
  INTEGER(i32),  PARAMETER:: Lower_to_Upper = ICHAR('A')-ICHAR('a')  ! = -32 for ASCII shift -> Add to lower to make upcase
  !
  INTEGER(i32),  PARAMETER::  LOWER_CHAR_SUM = ICHAR('a')+ICHAR('z')  ! Used for Inverse routines such that T = 'a' becomes T = 'z' with: 
  INTEGER(i32),  PARAMETER::  UPPER_CHAR_SUM = ICHAR('A')+ICHAR('Z')  !                                     T = CHAR( LOWER_CHAR_SUM - ICHAR(T) )
  INTEGER(i32),  PARAMETER:: NUMBER_CHAR_SUM = ICHAR('0')+ICHAR('9')  !
  !
  !---------------------------------------------------------------------------------------------------
  !
  ! Note that this is the IEEE x86_64 bit representation of NaN. 
  !
  REAL(DBL),     PARAMETER:: NaN = TRANSFER(-2251799813685248_i64, 1.0_DBL)  
  !
  ! This may not always be set to NaN, the safest method is to use 
  !   USE, INTRINSIC:: IEEE_ARITHMETIC, ONLY: IEEE_VALUE, IEEE_QUIET_NAN
  ! and set a local variable to 
  !  NaN = IEEE_VALUE(NaN, IEEE_QUIET_NAN)
  !
  !---------------------------------------------------------------------------------------------------
  !
  !
  REAL(SNG),     PARAMETER:: Z_SNG      =     0.0_sng
  REAL(SNG),     PARAMETER:: ONE_SNG    =     1.0_sng
  !
  !---------------------------------------------------------------------------------------------------
  !
  REAL(DBL),     PARAMETER:: PI         =     3.1415926535897930_dbl
  REAL(DBL),     PARAMETER:: TWOPI      =     6.2831853071795860_dbl
  REAL(DBL),     PARAMETER:: toRad      =     0.0174532925199433_dbl   ! PI/180
  REAL(DBL),     PARAMETER:: toDeg      =    57.2957795130824000_dbl   ! 180/PI
  !
  !---------------------------------------------------------------------------------------------------
  !
  REAL(DBL),     PARAMETER:: DNEG       =    -1.0_dbl
  REAL(DBL),     PARAMETER:: DZ         =     0.0_dbl
  REAL(DBL),     PARAMETER:: UNO        =     1.0_dbl
  REAL(DBL),     PARAMETER:: DOS        =     2.0_dbl
  REAL(DBL),     PARAMETER:: TRES       =     3.0_dbl
  REAL(DBL),     PARAMETER:: QUAD       =     4.0_dbl
  REAL(DBL),     PARAMETER:: CINCO      =     5.0_dbl
  REAL(DBL),     PARAMETER:: DIEZ       =    10.0_dbl
  REAL(DBL),     PARAMETER:: VIGINTI    =    20.0_dbl
  REAL(DBL),     PARAMETER:: SEXAGEN    =    60.0_dbl
  REAL(DBL),     PARAMETER:: HECTO      =   100.0_dbl
  REAL(DBL),     PARAMETER:: QUINCEN    =   500.0_dbl
  REAL(DBL),     PARAMETER:: KILO       =  1000.0_dbl
  !
  !---------------------------------------------------------------------------------------------------
  !
  REAL(DBL),     PARAMETER:: D3         =  1.0E3_dbl
  REAL(DBL),     PARAMETER:: D5         =  1.0E5_dbl
  REAL(DBL),     PARAMETER:: D6         =  1.0E6_dbl
  REAL(DBL),     PARAMETER:: D10        =  1.0E10_dbl
  REAL(DBL),     PARAMETER:: D29        =  1.0E29_dbl
  REAL(DBL),     PARAMETER:: D30        =  1.0E30_dbl
  REAL(DBL),     PARAMETER:: D40        =  1.0E40_dbl
  REAL(DBL),     PARAMETER:: D50        =  1.0E50_dbl
  REAL(DBL),     PARAMETER:: D99        =  1.0E99_dbl
  REAL(DBL),     PARAMETER:: D100       = 1.0E100_dbl
  REAL(DBL),     PARAMETER:: D200       = 1.0E200_dbl
  REAL(DBL),     PARAMETER:: D250       = 1.0E250_dbl
  REAL(DBL),     PARAMETER:: D300       = 1.0E300_dbl
  !
  !---------------------------------------------------------------------------------------------------
  !
  REAL(DBL),     PARAMETER:: DODRANT    = 0.75_dbl
  REAL(DBL),     PARAMETER:: HALF       = 0.5_dbl
  REAL(DBL),     PARAMETER:: THIRD      = 0.33333333333333333_dbl
  REAL(DBL),     PARAMETER:: FOURTH     = 0.25_dbl
  REAL(DBL),     PARAMETER:: FIFTH      = 0.2_dbl
  REAL(DBL),     PARAMETER:: TENTH      = 0.1_dbl
  REAL(DBL),     PARAMETER:: CENTI      = 0.01_dbl
  REAL(DBL),     PARAMETER:: MILLI      = 0.001_dbl
  REAL(DBL),     PARAMETER:: DECIMILLI  = 0.0001_dbl
  !
  !---------------------------------------------------------------------------------------------------
  !
  REAL(DBL),     PARAMETER:: negD100    = -1.0E100_dbl
  REAL(DBL),     PARAMETER:: negD200    = -1.0E200_dbl
  REAL(DBL),     PARAMETER:: negD250    = -1.0E250_dbl
  REAL(DBL),     PARAMETER:: negD300    = -1.0E300_dbl
  !
  !---------------------------------------------------------------------------------------------------
  !
  REAL(DBL),     PARAMETER:: YEARTOL_05 = 1.590E-8_dbl  ! ~ 0.5 second in years
  REAL(DBL),     PARAMETER:: YEARTOL    = 3.960E-8_dbl  ! ~ 1.25 seconds in years
  REAL(DBL),     PARAMETER:: YEARTOL_1  = 3.190E-8_dbl  ! ~ 1.0 second in years
  REAL(DBL),     PARAMETER:: YEARTOL_5  = 1.590E-7_dbl  ! ~ 5.0 second in years
  REAL(DBL),     PARAMETER:: YEARTOL_10 = 3.175E-7_dbl  ! ~ 10. second in years
  REAL(DBL),     PARAMETER:: YEARTOL_60 = 1.905E-6_dbl  ! ~ 60. second in years
  !
  !---------------------------------------------------------------------------------------------------
  ! FOR CHECKING < DZ
  REAL(DBL),     PARAMETER:: NEARZERO_3  = 1.0E-3_dbl
  REAL(DBL),     PARAMETER:: NEARZERO_5  = 1.0E-5_dbl
  REAL(DBL),     PARAMETER:: NEARZERO_6  = 1.0E-6_dbl
  REAL(DBL),     PARAMETER:: NEARZERO_7  = 1.0E-7_dbl
  REAL(DBL),     PARAMETER:: NEARZERO_10 = 1.0E-10_dbl
  REAL(DBL),     PARAMETER:: NEARZERO_12 = 1.0E-12_dbl  !COMMON PRECISION ERROR
  REAL(DBL),     PARAMETER:: NEARZERO_14 = 1.0E-14_dbl
  REAL(DBL),     PARAMETER:: NEARZERO_15 = 1.0E-15_dbl
  REAL(DBL),     PARAMETER:: NEARZERO_20 = 1.0E-20_dbl
  REAL(DBL),     PARAMETER:: NEARZERO_29 = 1.0E-29_dbl  !SNGL LIMIT ~1E-38, DBLE LIMT ~1D-308
  REAL(DBL),     PARAMETER:: NEARZERO_30 = 1.0E-30_dbl
  !
  !---------------------------------------------------------------------------------------------------
  ! FOR CHECKING > UNO  
  REAL(DBL),     PARAMETER:: NEAR_ONE    = 1.000000001_dbl  !NEAR_ONE_9
  REAL(DBL),     PARAMETER:: NEAR_ONE_3  = 1.001_dbl
  REAL(DBL),     PARAMETER:: NEAR_ONE_5  = 1.00001_dbl
  REAL(DBL),     PARAMETER:: NEAR_ONE_7  = 1.0000001_dbl
  REAL(DBL),     PARAMETER:: NEAR_ONE_10 = 1.0000000001_dbl
  !
  ! FOR CHECKING > UNO  
  REAL(DBL),     PARAMETER:: SUB_ONE     = 0.999999999_dbl  !NEAR_ONE_9
  REAL(DBL),     PARAMETER:: SUB_ONE_3   = 0.999_dbl
  REAL(DBL),     PARAMETER:: SUB_ONE_5   = 0.99999_dbl
  REAL(DBL),     PARAMETER:: SUB_ONE_7   = 0.9999999_dbl
  REAL(DBL),     PARAMETER:: SUB_ONE_10  = 0.9999999999_dbl
  !
  !---------------------------------------------------------------------------------------------------
  !
  REAL(DBL),     PARAMETER:: NEGNEARZERO_3  = -1.0E-3_dbl
  REAL(DBL),     PARAMETER:: NEGNEARZERO_5  = -1.0E-5_dbl
  REAL(DBL),     PARAMETER:: NEGNEARZERO_6  = -1.0E-6_dbl
  REAL(DBL),     PARAMETER:: NEGNEARZERO_7  = -1.0E-7_dbl
  REAL(DBL),     PARAMETER:: NEGNEARZERO_8  = -1.0E-8_dbl
  REAL(DBL),     PARAMETER:: NEGNEARZERO_10 = -1.0E-10_dbl
  REAL(DBL),     PARAMETER:: NEGNEARZERO_12 = -1.0E-12_dbl
  REAL(DBL),     PARAMETER:: NEGNEARZERO_15 = -1.0E-15_dbl
  REAL(DBL),     PARAMETER:: NEGNEARZERO_20 = -1.0E-20_dbl
  REAL(DBL),     PARAMETER:: NEGNEARZERO_30 = -1.0E-30_dbl
  !
  !---------------------------------------------------------------------------------------------------
  !
  INTEGER(i32),  PARAMETER:: inf_I       =  HUGE(Z)
  REAL(DBL),     PARAMETER:: inf         =  HUGE(DZ)
  REAL(DBL),     PARAMETER:: ninf        = -HUGE(DZ)
  INTEGER(i32),  PARAMETER:: ninf_I      = -HUGE(Z)
  !
  REAL(SNG),     PARAMETER::  inf_R      =  HUGE(1.0_sng)
  REAL(SNG),     PARAMETER:: ninf_R      = -HUGE(inf_R)
  !              
  REAL(DBL),     PARAMETER:: SNGL_inf    =  3.4E38_dbl   !Close to Single Precision INF =  3.4028235E+38
  REAL(DBL),     PARAMETER:: SNGL_ninf   = -3.4E38_dbl   !Close to Single Precision INF = -3.4028235E+38
  REAL(SNG),     PARAMETER:: SNGL_inf_R  =  3.4E38_sng   !dito
  REAL(SNG),     PARAMETER:: SNGL_ninf_R = -3.4E38_sng   !dito
  !
  REAL(DBL),     PARAMETER:: NEAR_inf    = inf*0.99999_dbl
  INTEGER(i32),  PARAMETER:: NEAR_inf_I  = inf_I - EIGHT
  !
  REAL(DBL),     PARAMETER:: NEAR_ninf   = ninf*0.99999_dbl
  !
  !---------------------------------------------------------------------------------------------------
  !
  REAL(SNG),     PARAMETER:: LOG_2_R     = LOG(2.0_SNG)
  REAL(DBL),     PARAMETER:: LOG_2       = LOG(2.0_DBL)
  !
  !---------------------------------------------------------------------------------------------------
  !
  INTEGER(i64),  PARAMETER:: LONG_NEG2   = -2_i64
  INTEGER(i64),  PARAMETER:: LONG_NEG    = -1_i64
  INTEGER(i64),  PARAMETER:: LONG_ZER    =  0_i64
  INTEGER(i64),  PARAMETER:: LONG_ONE    =  1_i64
  INTEGER(i64),  PARAMETER:: LONG_TWO    =  2_i64
  INTEGER(i64),  PARAMETER:: LONG_TEN    = 10_i64
  !
  INTEGER(i64),  PARAMETER:: LONG_inf_I  =  INT( inf_I, i64)
  INTEGER(i64),  PARAMETER:: LONG_ninf_I =  INT(ninf_I, i64)
  !
  !#########################################################################
  !
  INTERFACE GET_NAN  !MAKE X = NaN ==> X = GET_NAN(X)
    MODULE PROCEDURE GET_REAL_NAN  !(X)   X => REAL
    MODULE PROCEDURE GET_DBLE_NAN  !(X)   X => DOUBLE PRECISION
  END INTERFACE
  !
  !------------------------------------------------------------------------
  !
  INTERFACE SET_NAN  !MAKE X = NaN ==> CALL SET_NAN(X)
    MODULE PROCEDURE SET_REAL_NAN!(X)
    MODULE PROCEDURE SET_DBLE_NAN!(X)
  END INTERFACE
  !
  !#########################################################################
  !
  CONTAINS
  !
  IMPURE ELEMENTAL FUNCTION GET_REAL_NAN(X) RESULT(NaN)
     REAL(SNG), INTENT(IN):: X
     REAL(SNG):: NaN
     CALL SET_REAL_NAN(NaN)
  END FUNCTION
  !
  IMPURE ELEMENTAL FUNCTION GET_DBLE_NAN(X) RESULT(NaN)
     REAL(DBL), INTENT(IN):: X
     REAL(DBL):: NaN
     CALL  SET_DBLE_NAN(NaN)
  END FUNCTION 
  !
  IMPURE ELEMENTAL SUBROUTINE SET_REAL_NAN(NaN)
     REAL(SNG), INTENT(OUT):: NaN
     REAL(SNG),        SAVE:: NaN_REL=0.0
     LOGICAL,             SAVE:: SET_REL=TRUE
     !
     IF(SET_REL) THEN
        SET_REL = FALSE
                           BLOCK
                                CHARACTER(3):: CNAN
                                CNAN = 'NaN'
                                READ(CNAN,*) NaN_REL
                           END BLOCK
     END IF
     !
     NaN = NaN_REL
     !
  END SUBROUTINE  
  !
  IMPURE ELEMENTAL SUBROUTINE SET_DBLE_NAN(NaN)
     REAL(DBL), INTENT(OUT):: NaN
     REAL(DBL),        SAVE:: NaN_DBL = DZ
     LOGICAL,                 SAVE:: SET_DBL  = TRUE
     !
     IF(SET_DBL) THEN
        SET_DBL = FALSE
                           BLOCK
                                CHARACTER(3):: CNAN
                                CNAN = 'NaN'
                                READ(CNAN,*) NaN_DBL
                           END BLOCK
     END IF
     !
     NaN = NaN_DBL
     !
  END SUBROUTINE  
  !
END MODULE
!
!