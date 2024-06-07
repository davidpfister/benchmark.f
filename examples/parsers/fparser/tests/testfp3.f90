PROGRAM fptest
  !--------- -------- --------- --------- --------- --------- --------- --------- -----
  !
  ! Example program 3 for using the function parser module:
  !
  ! Assessing how fast the interpreter is compared against a direct evaluation 
  !
  !--------- -------- --------- --------- --------- --------- --------- --------- -----
  USE parameters, ONLY: rn
  USE fparser
  IMPLICIT NONE
  INTEGER,                             PARAMETER :: neval = 1000000
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
  INTEGER                                        :: i,n
  REAL                                           :: rt1,rt2,rt3
  REAL(rn)                                       :: vel,alpha,beta
  !--------- -------- --------- --------- --------- --------- --------- --------- -----
  !
  CALL initf (nfunc)                      ! Initialize function parser for nfunc functions
  DO i=1,nfunc
     CALL parsef (i, func(i), var)        ! Parse and bytecompile ith function string 
  END DO
  vel   = val(1)
  alpha = val(2)
  beta  = val(3)
  CALL CPU_TIME (rt1)
  DO n=1,neval
     DO i=1,nfunc
        res = evalf (i, val)              ! Interprete bytecode representation of ith function
        IF (EvalErrType > 0) WRITE(*,*)'*** Error: ',EvalErrMsg ()
     END DO
  END DO
  CALL CPU_TIME (rt2)
  DO n=1,neval
     res = vel*COS(beta)
     res = vel*SIN(beta)*COS(alpha)
     res = vel*SIN(beta)*SIN(alpha)
  END DO
  CALL CPU_TIME (rt3)
  WRITE(*,*)'Function evaluation:'
  WRITE(*,*)'- Bytecode interpreter cpu time = ',rt2-rt1
  WRITE(*,*)'- Machine code         cpu time = ',rt3-rt2,' = ',(rt3-rt2)/(rt2-rt1)*100.,'%'
  !
END PROGRAM fptest
