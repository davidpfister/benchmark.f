PROGRAM fptest
  !--------- -------- --------- --------- --------- --------- --------- --------- -----
  !
  ! Example program 1 for using the function parser module
  !
  !--------- -------- --------- --------- --------- --------- --------- --------- -----
  USE parameters, ONLY: rn
  USE fparser,    ONLY: initf, parsef, evalf, EvalErrType, EvalErrMsg
  IMPLICIT NONE
  INTEGER,                             PARAMETER :: nfunc = 1
  CHARACTER (LEN=*), DIMENSION(nfunc), PARAMETER :: func = (/ '-x' /)
  INTEGER,                             PARAMETER :: nvar = 1
  CHARACTER (LEN=*), DIMENSION(nvar),  PARAMETER :: var  = (/ 'x'  /)
  REAL(rn),          DIMENSION(nvar),  PARAMETER :: val  = (/  2.  /)
  REAL(rn)                                       :: res
  INTEGER                                        :: i
  REAL(rn)                                       :: x
  !--------- -------- --------- --------- --------- --------- --------- --------- -----
  !
  CALL initf (nfunc)                      ! Initialize function parser for nfunc functions
  DO i=1,nfunc
     CALL parsef (i, func(i), var)        ! Parse and bytecompile ith function string 
  END DO
  WRITE(*,*)'-----------------------------------------------------------------------------'
  WRITE(*,*)'==> Bytecode evaluation:'
  DO i=1,nfunc
     res = evalf (i, val)                 ! Interprete bytecode representation of ith function
     IF (EvalErrType > 0) WRITE(*,*)'*** Error: ',EvalErrMsg ()
     WRITE(*,*) func(i),'=',res
  END DO
  WRITE(*,*)'==> Direct evaluation:'
  x  = val(1)
  WRITE(*,*)'-x=',-x
  !
END PROGRAM fptest
