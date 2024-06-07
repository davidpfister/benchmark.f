PROGRAM fptest
  !--------- -------- --------- --------- --------- --------- --------- --------- -----
  !
  ! Example program 1 for using the function parser module
  !
  !--------- -------- --------- --------- --------- --------- --------- --------- -----
  USE parameters, ONLY: rn
  USE fparser,    ONLY: initf, parsef, evalf, EvalErrType, EvalErrMsg
  IMPLICIT NONE
  INTEGER,                             PARAMETER :: nfunc = 4
  CHARACTER (LEN=*), DIMENSION(nfunc), PARAMETER :: func = (/ '-1.0*x        ',  &
                                                              '-x            ',  &
                                                              'A*COS(B*x)+5  ',  &
                                                              'A*COS(B*x)+5.0' /)
  INTEGER,                             PARAMETER :: nvar = 3
  CHARACTER (LEN=*), DIMENSION(nvar),  PARAMETER :: var  = (/ 'x', &
                                                              'A', &
                                                              'B'  /)
  REAL(rn),          DIMENSION(nvar),  PARAMETER :: val  = (/  2., 3., 4. /)
  REAL(rn)                                       :: res
  INTEGER                                        :: i
  REAL(rn)                                       :: x,A,B
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
  A  = val(2)
  B  = val(3)
  WRITE(*,*)'-1.0*x        =',-1.0*x
  WRITE(*,*)'-x            =',-x
  WRITE(*,*)'A*COS(B*x)+5  =',A*COS(B*x)+5
  WRITE(*,*)'A*COS(B*X)+5.0=',A*COS(B*X)+5.0
  !
END PROGRAM fptest
