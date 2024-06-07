PROGRAM fptest
  !--------- -------- --------- --------- --------- --------- --------- --------- -----
  !
  ! Example program 2 for using the function parser module
  !
  !--------- -------- --------- --------- --------- --------- --------- --------- -----
  USE parameters, ONLY: rn
  USE fparser
  IMPLICIT NONE
  INTEGER,                             PARAMETER :: nfunc = 3
  CHARACTER (LEN=*), DIMENSION(nfunc), PARAMETER :: func = (/ 'vel*COS(beta)           ', &
                                                              'vel*SIN(beta)*COS(alpha)', &
                                                              'vel*SIN(beta)*SIN(alpha)' /)
  INTEGER,                             PARAMETER :: nvar = 3
  CHARACTER (LEN=*), DIMENSION(nvar),  PARAMETER :: var  = (/ 'vel  ', &
                                                              'alpha', &
                                                              'beta ' /)
  REAL(rn),          DIMENSION(nvar),  PARAMETER :: val  = (/  10., 1.5, 2.0  /)
  REAL(rn)                                       :: res
  INTEGER                                        :: i
  REAL(rn)                                       :: vel,alpha,beta
  !--------- -------- --------- --------- --------- --------- --------- --------- -----
  !
  CALL initf (nfunc)                      ! Initialize function parser for nfunc functions
  DO i=1,nfunc
     CALL parsef (i, func(i), var)        ! Parse and bytecompile ith function string 
  END DO
  WRITE(*,*)'==> Bytecode evaluation:'
  DO i=1,nfunc
     res = evalf (i, val)                 ! Interprete bytecode representation of ith function
     IF (EvalErrType > 0) WRITE(*,*)'*** Error: ',EvalErrMsg ()
     WRITE(*,*)'res=',res
  END DO
  WRITE(*,*)'==> Direct evaluation:'
  vel   = val(1)
  alpha = val(2)
  beta  = val(3)
  WRITE(*,*)'res=',vel*COS(beta)
  WRITE(*,*)'res=',vel*SIN(beta)*COS(alpha)
  WRITE(*,*)'res=',vel*SIN(beta)*SIN(alpha)
  !
END PROGRAM fptest
