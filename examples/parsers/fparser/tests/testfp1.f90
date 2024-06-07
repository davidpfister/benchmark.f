PROGRAM fptest
  !--------- -------- --------- --------- --------- --------- --------- --------- -----
  !
  ! Example program 1 for using the function parser module
  !
  !--------- -------- --------- --------- --------- --------- --------- --------- -----
  USE parameters, ONLY: rn
  USE fparser,    ONLY: initf, parsef, evalf, EvalErrType, EvalErrMsg
  IMPLICIT NONE
  INTEGER,                             PARAMETER :: nfunc = 3
  CHARACTER (LEN=*), DIMENSION(nfunc), PARAMETER :: func = (/ 'a0*b0 ', &
                                                              'a1/b1 ', &
                                                              'a3**b3' /)
  INTEGER,                             PARAMETER :: nvar = 6
  CHARACTER (LEN=*), DIMENSION(nvar),  PARAMETER :: var  = (/ 'a0', &
                                                              'b0', &
                                                              'a1', &
                                                              'b1', &
                                                              'a3', &
                                                              'b3' /)
  REAL(rn),          DIMENSION(nvar),  PARAMETER :: val  = (/  1., 2., 3., 0., 5., 6. /)
  REAL(rn)                                       :: res
  INTEGER                                        :: i
  REAL(rn)                                       :: a0,b0,a1,b1,a3,b3
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
  a0 = val(1)
  b0 = val(2)
  a1 = val(3)
  b1 = val(4)
  a3 = val(5)
  b3 = val(6)
  WRITE(*,*)'res=',a0*b0
  WRITE(*,*)'res=',a1/b1
  WRITE(*,*)'res=',a3**b3
  !
END PROGRAM fptest
