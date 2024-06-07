PROGRAM fptest
  !--------- -------- --------- --------- --------- --------- --------- --------- -----
  !
  ! Example program 4 for using the function parser module
  !
  !--------- -------- --------- --------- --------- --------- --------- --------- -----
  USE parameters, ONLY: rn
  USE fparser
  IMPLICIT NONE
  INTEGER,                             PARAMETER :: nfunc = 1
  CHARACTER (LEN=*), DIMENSION(nfunc), PARAMETER :: func = (/ '1.0e0 + 5.e1' /)
  INTEGER,                             PARAMETER :: nvar = 0
  CHARACTER (LEN=*), DIMENSION(nvar),  PARAMETER :: var  = 'a'
  REAL(rn),          DIMENSION(nvar),  PARAMETER :: val  = 0._rn
  REAL(rn)                                       :: res
  INTEGER                                        :: i
  !--------- -------- --------- --------- --------- --------- --------- --------- -----
  !
  CALL initf (nfunc)                      ! Initialize function parser for nfunc functions
  DO i=1,nfunc
     WRITE(*,*)'UP parsef'
     CALL parsef (i, func(i), var)        ! Parse and bytecompile ith function string 
  END DO
  DO i=1,nfunc
     WRITE(*,*)'FCN evalf'
     res = evalf (i, val)                 ! Interprete bytecode representation of ith function
     IF (EvalErrType > 0) WRITE(*,*)'*** Error: ',EvalErrMsg ()
     WRITE(*,*)'res=',res
  END DO
  !
END PROGRAM fptest
