module checkfn

  use interpreter

  implicit none

  integer, parameter :: fdp = 8
  integer, save :: count = 0
  real(fdp), parameter :: zero = tiny(zero)

contains

  subroutine s_checkfn(f, s, varvals, eerr, perr, eeerr, res, reserr, tol)
    character(len=255), intent(in) :: f, s
    real(fdp), dimension(:), intent(in) :: varvals
    character(len=*), optional, intent(in) :: eerr, perr, eeerr
    real(fdp), optional, intent(in) :: res, reserr, tol
    character(len=255) :: error1, error2, error3
    real(fdp) :: r, rerr, ttol
    integer :: funcnum

    count = count + 1

    call s_createfn(f, s, funcnum, error1)
    call s_evaluatefn(funcnum, varvals, r, error2)
    call s_evaluateerr(funcnum, varvals, rerr, error3)
    
    if (present(tol)) then
       ttol = tol
    else
       ttol = epsilon(r)
    end if

    if (present(perr)) then
       if (trim(error1) == trim(perr)) then
          write(*,'(i4.4,a)') count, '  Testing(PARSE ERROR)     :  PASSED'
       else
          write(*,'(a)') '==============================='
          write(*, '(a,i4.4)') 'Test(ERROR): ', count
          write(*,'(a,a)') 'Func: ', trim(f)
          write(*,'(a,i4.4)') 'Func number: ',funcnum
          write(*,'(a,a,a)') 'Parse error: "',trim(error1),'"'
          write(*,'(a,a,a)') 'Evaluate error: "', trim(error2), '"'
          write(*,'(a,a,a)') 'Evaluate error, error: "', trim(error3), '"'
          write(*,'(a,e18.10e3)') 'Evaluate value: ', r
          write(*,'(a,e18.10e3)') 'Evaluate value error: ', rerr
          write(*,'(a)') '**** ERROR:         FAIL'
          write(*,'(a,a,a)') '**** Expecting: "', trim(perr),'"'
          write(*,'(a)') '==============================='
       end if
    end if

    if (present(eerr)) then
       if (trim(error2) == trim(eerr)) then
          write(*,'(i4.4,a)') count, '  Testing(ERROR)           :  PASSED'
       else
          write(*,'(a)') '==============================='
          write(*, '(a,i4.4)') 'Test(ERROR): ', count
          write(*,'(a,a)') 'Func: ', trim(f)
          write(*,'(a,i4.4)') 'Func number: ',funcnum
          write(*,'(a,a,a)') 'Parse error: "',trim(error1),'"'
          write(*,'(a,a,a)') 'Evaluate error: "', trim(error2), '"'
          write(*,'(a,a,a)') 'Evaluate error, error: "', trim(error3), '"'
          write(*,'(a,e18.10e3)') 'Evaluate value: ', r
          write(*,'(a,e18.10e3)') 'Evaluate value error: ', rerr
          write(*,'(a)') '**** ERROR:         FAIL'
          write(*,'(a,a,a)') '**** Expecting: "', trim(eerr),'"'
          write(*,'(a)') '==============================='
       end if
    end if

    if (present(eeerr)) then
       if (trim(error3) == trim(eeerr)) then
          write(*,'(i4.4,a)') count, '  Testing(ERROR)           :  PASSED'
       else
          write(*,'(a)') '==============================='
          write(*, '(a,i4.4)') 'Test(ERROR): ', count
          write(*,'(a,a)') 'Func: ', trim(f)
          write(*,'(a,i4.4)') 'Func number: ',funcnum
          write(*,'(a,a,a)') 'Parse error: "',trim(error1),'"'
          write(*,'(a,a,a)') 'Evaluate error: "', trim(error2), '"'
          write(*,'(a,a,a)') 'Evaluate error, error: "', trim(error3), '"'
          write(*,'(a,e18.10e3)') 'Evaluate value: ', r
          write(*,'(a,e18.10e3)') 'Evaluate value error: ', rerr
          write(*,'(a)') '**** ERROR:         FAIL'
          write(*,'(a,a,a)') '**** Expecting: "', trim(eerr),'"'
          write(*,'(a)') '==============================='
       end if
    end if

    if (present(res)) then
       if (error2 == 'OK' .and. abs(1.0d0-(r+zero)/(res+zero)) <= ttol) then
          write(*,'(i4.4,a)') count, '  Testing(EVALUATION)      :  PASSED'
       else
          write(*,'(a)') '==============================='
          write(*, '(a,i4.4)') 'Test(EVALUATION): ', count
          write(*,'(a,a)') 'Func: ', trim(f)
          write(*,'(a,i4.4)') 'Func number: ',funcnum
          write(*,'(a,a,a)') 'Parse error: "',trim(error1),'"'
          write(*,'(a,a,a)') 'Evaluate error: "', trim(error2), '"'
          write(*,'(a,a,a)') 'Evaluate error, error: "', trim(error3), '"'
          write(*,'(a,e18.10e3)') 'Evaluate value: ', r
          write(*,'(a,e18.10e3)') 'Evaluate value error: ', rerr
          write(*,'(a)') '**** EVALUATION:    FAIL'
          write(*,'(a,e18.10e3)') '**** Expecting: ', res
          write(*,'(a,e18.10e3)') '**** Difference: ', res-r
          write(*,'(a)') '==============================='
       end if
    end if

    if (present(reserr)) then
       if (error3 == 'OK' .and. abs(1.0d0-(rerr+zero)/(reserr+zero)) <= ttol) then
          write(*,'(i4.4,a)') count, '  Testing(EVALUATION ERROR):  PASSED '
       else
          write(*,'(a)') '==============================='
          write(*, '(a,i4.4)') 'Test(EVALUATION ERROR): ', count
          write(*,'(a,a)') 'Func: ', trim(f)
          write(*,'(a,i4.4)') 'Func number: ',funcnum
          write(*,'(a,a,a)') 'Parse error: "',trim(error1),'"'
          write(*,'(a,a,a)') 'Evaluate error: "', trim(error2), '"'
          write(*,'(a,a,a)') 'Evaluate error, error: "', trim(error3), '"'
          write(*,'(a,e18.10e3)') 'Evaluate value: ', r
          write(*,'(a,e18.10e3)') 'Evaluate value error: ', rerr
          write(*,'(a)') '**** EVALUATION ERROR:    FAIL'
          write(*,'(a,e18.10e3)') '**** Expecting: ', reserr
          write(*,'(a,e18.10e3)') '**** Difference: ', reserr-rerr
          write(*,'(a)') '==============================='
       end if
    end if

    call s_destroyfn(funcnum)
  end subroutine s_checkfn

end module checkfn



program check
  use extrafunc
  use interpreter
  use checkfn

  implicit none

  real(fdp), parameter :: inf = huge(inf)
  character(len=255) :: s, f
  character(len=255) :: error
  real(fdp) :: w, x, y, z, r, e
  real(fdp) :: a(100)
  integer :: i


  !
  ! Test 1
  !
  f = '3.14159265359'
  s = ''
  r = 3.14159265359d0
  call s_checkfn(f, s, (/0.0d0/), res=r)

  !
  ! Test 2
  !
  f = 'x'
  s = 'x'
  x = 3.14159265359d0
  r = x
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 3
  !
  f = 'x**y'
  s = 'x y'
  x = 2.0d0
  y = 3.0d0
  r = x**y
  call s_checkfn(f, s, (/x, y/), res=r)

  !
  ! Test 4
  !
  f = 'x!=y'
  s = 'x y'
  x = 2.0d0
  y = 3.0d0
  r = 1.0d0
  call s_checkfn(f, s, (/x, y/), res=r)

  !
  ! Test 5
  !
  f = 'x!=y'
  s = 'x y'
  x = 2.0d0
  y = 2.0d0
  r = 0.0d0
  call s_checkfn(f, s, (/x, y/), res=r)

  !
  ! Test 6
  !
  f = 'x==y'
  s = 'x y'
  x = 3.0d0
  y = 3.0d0
  r = 1.0d0
  call s_checkfn(f, s, (/x, y/), res=r)

  !
  ! Test 7
  !
  f = 'x==y'
  s = 'x y'
  x = 3.0d0
  y = 2.0d0
  r = 0.0d0
  call s_checkfn(f, s, (/x, y/), res=r)

  !
  ! Test 8
  !
  f = 'x>=y'
  s = 'x y'
  x = 4.0d0
  y = 3.0d0
  r = 1.0d0
  call s_checkfn(f, s, (/x, y/), res=r)

  !
  ! Test 9
  !
  f = 'x>=y'
  s = 'x y'
  x = 3.0d0
  y = 4.0d0
  r = 0.0d0
  call s_checkfn(f, s, (/x, y/), res=r)

  !
  ! Test 10
  !
  f = 'x<=y'
  s = 'x y'
  x = 2.0d0
  y = 3.0d0
  r = 1.0d0
  call s_checkfn(f, s, (/x, y/), res=r)

  !
  ! Test 11
  !
  f = 'x<=y'
  s = 'x y'
  x = 3.0d0
  y = 2.0d0
  r = 0.0d0
  call s_checkfn(f, s, (/x, y/), res=r)

  !
  ! Test 12
  !
  f = 'x>y'
  s = 'x y'
  x = 4.0d0
  y = 3.0d0
  r = 1.0d0
  call s_checkfn(f, s, (/x, y/), res=r)

  !
  ! Test 13
  !
  f = 'x>y'
  s = 'x y'
  x = 3.0d0
  y = 4.0d0
  r = 0.0d0
  call s_checkfn(f, s, (/x, y/), res=r)

  !
  ! Test 14
  !
  f = 'x<y'
  s = 'x y'
  x = 2.0d0
  y = 3.0d0
  r = 1.0d0
  call s_checkfn(f, s, (/x, y/), res=r)

  !
  ! Test 15
  !
  f = 'x<y'
  s = 'x y'
  x = 3.0d0
  y = 2.0d0
  r = 0.0d0
  call s_checkfn(f, s, (/x, y/), res=r)

  !
  ! Test 16
  !
  f = 'x*y'
  s = 'x y'
  x = 3.0d0
  y = 2.0d0
  r = x*y
  call s_checkfn(f, s, (/x, y/), res=r)

  !
  ! Test 17
  !
  f = 'x/y'
  s = 'x y'
  x = 3.0d0
  y = 2.0d0
  r = x/y
  call s_checkfn(f, s, (/x, y/), res=r)

  !
  ! Test 18
  !
  f = 'x/y'
  s = 'x y'
  x = 3.0d0
  y = 0.0d0
  error = 'x/y, y=0'
  call s_checkfn(f, s, (/x, y/), eerr=error)

  !
  ! Test 19
  !
  f = 'x+y'
  s = 'x y'
  x = 3.0d0
  y = 2.0d0
  r = x+y
  call s_checkfn(f, s, (/x, y/), res=r)

  !
  ! Test 20
  !
  f = 'x-y'
  s = 'x y'
  x = 3.0d0
  y = 2.0d0
  r = x-y
  call s_checkfn(f, s, (/x, y/), res=r)

  !
  ! Test 21
  !
  f = 'cos(x)'
  s = 'x'
  x = 3.0d0
  r = cos(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 22
  !
  f = 'sin(x)'
  s = 'x'
  x = 3.0d0
  r = sin(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 23
  !
  f = 'tan(x)'
  s = 'x'
  x = 3.0d0
  r = tan(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 24
  !
  f = 'exp(x)'
  s = 'x'
  x = 3.0d0
  r = exp(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 25
  !
  f = 'log(x)'
  s = 'x'
  x = 3.0d0
  r = log(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 26
  !
  f = 'log(x)'
  s = 'x'
  x = -3.0d0
  error = 'log(x), x<=0'
  call s_checkfn(f, s, (/x/), eerr=error)

  !
  ! Test 27
  !
  f = 'log10(x)'
  s = 'x'
  x = 3.0d0
  r = log10(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 28
  !
  f = 'log10(x)'
  s = 'x'
  x = -3.0d0
  error = 'log10(x), x<=0'
  call s_checkfn(f, s, (/x/), eerr=error)

  !
  ! Test 29
  !
  f = 'sqrt(x)'
  s = 'x'
  x = 3.0d0
  r = sqrt(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 30
  !
  f = 'sqrt(x)'
  s = 'x'
  x = -3.0d0
  error = 'sqrt(x), x<0'
  call s_checkfn(f, s, (/x/), eerr=error)

  !
  ! Test 31
  !
  f = 'acos(x)'
  s = 'x'
  x = 0.3d0
  r = acos(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 32
  !
  f = 'acos(x)'
  s = 'x'
  x = -3.0d0
  error = 'acos(x), |x|>1'
  call s_checkfn(f, s, (/x/), eerr=error)

  !
  ! Test 33
  !
  f = 'asin(x)'
  s = 'x'
  x = 0.3d0
  r = asin(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 34
  !
  f = 'asin(x)'
  s = 'x'
  x = -3.0d0
  error = 'asin(x), |x|>1'
  call s_checkfn(f, s, (/x/), eerr=error)

  !
  ! Test 35
  !
  f = 'atan(x)'
  s = 'x'
  x = 0.3d0
  r = atan(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 36
  !
  f = 'cosh(x)'
  s = 'x'
  x = 0.3d0
  r = cosh(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 37
  !
  f = 'sinh(x)'
  s = 'x'
  x = 0.3d0
  r = sinh(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 38
  !
  f = 'tanh(x)'
  s = 'x'
  x = 0.3d0
  r = tanh(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 39
  !
  f = 'anint(x)'
  s = 'x'
  x = 1.3d0
  r = anint(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 40
  !
  f = 'aint(x)'
  s = 'x'
  x = 1.7d0
  r = aint(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 41
  !
  f = 'abs(x)'
  s = 'x'
  x = -1.7d0
  r = abs(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 42
  !
  f = 'delta(x)'
  s = 'x'
  x = 0.0d0
  r = 1.0d0
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 43
  !
  f = 'step(x)'
  s = 'x'
  x = 1.0d0
  r = 1.0d0
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 44
  !
  f = 'hat(x)'
  s = 'x'
  x = 1.0d0
  r = 0.0d0
  call s_checkfn(f, s, (/x/), res=r)

  !
  !  Test 45
  !
  f = '(x +y+z+x*y +x*z+y*z+x/y +x /z+y/z+x*cos ( x)    +y*sin(y)+z*tan(z)&
       &)**2/(x+y+z+x*y+x*z+y*z+x/y+x/z+y/z+x*cos(x)+y*sin(y)+z*tan(z))**3&
       &+sqrt(x*y*z+x+y+z)*exp(sqrt(x**2+y**2+z**2)+x+y+z)'
  s = 'x y z'
  x = 1.1d0
  y = 2.2d0
  z = 3.3d0
  r = (x +y+z+x*y +x*z+y*z+x/y +x /z+y/z+x*cos ( x)    +y*sin(y)+z*tan(z)&
       &)**2/(x+y+z+x*y+x*z+y*z+x/y+x/z+y/z+x*cos(x)+y*sin(y)+z*tan(z))**3&
       &+sqrt(x*y*z+x+y+z)*exp(sqrt(x**2+y**2+z**2)+x+y+z)
  call s_checkfn(f, s, (/x, y, z/), res=r)

  !
  !  Test 46
  !
  f = 'cos(x)+sin(y)+z**2'
  s = 'x y z'
  x = 1.1d0
  y = 2.2d0
  z = 3.3d0
  r = cos(x)+sin(y)+z**2
  call s_checkfn(f, s, (/x, y, z/), res=r, tol=1.0d-10)

  !
  !  Test 47
  !
  f = 'x+y*z+1-cos(x**y)'
  s = 'x y z'
  x = 1.1d0
  y = 2.2d0
  z = 3.3d0
  r = x+y*z+1-cos(x**y)
  call s_checkfn(f, s, (/x, y, z/), res=r, tol=1.0d-10)

  !
  ! Test 48
  !
  f = '7.1323e-05'
  s = ''
  r = 7.1323d-05
  call s_checkfn(f, s, (/0.0d0/), res=r)

  !
  ! Test 49
  !
  f = '7.13230000E-05'
  s = ''
  r = 7.1323d-05
  call s_checkfn(f, s, (/0.0d0/), res=r)

  !
  ! Test 50
  !
  f = 'x+y'
  s = 'x[1] y'
  x = 1.0d0
  y = 2.0d0
  r = x+y
  call s_checkfn(f, s, (/x, y/), res=r)

  !
  ! Test 51
  !
  f = '(((a+b)/(a+b)-exp(b)+exp(b))/2.0)-const'
  s = 'a b const'
  x = 2.d0
  y = 1.5d0
  z = -1.d0
  r = (((x+y)/(x+y)-exp(y)+exp(y))/2.0d0)-z
  call s_checkfn(f, s, (/x, y, z/), res=r)

  !
  ! Test 52
  !
  f = '((x+y)*z'
  s = 'x y z'
  x = 1.0d0
  y = 2.0d0
  z = 3.0d0
  error='Unbalanced brackets: ((x+y)*z'
  call s_checkfn(f, s, (/x, y, z/), perr=error)

  !
  ! Test 53
  !
  f = 'x+y'
  s = 'x[1.0] y'
  x = 1.0d0
  y = 1.0d0
  r = 2.0d0
  call s_checkfn(f, s, (/x, y/), res=r)

  !
  ! Test 54
  !
  f = 'x+y'
  s = 'x y[1.3d0]'
  x = 1.0d0
  y = 1.0d0
  r = 2.0d0
  call s_checkfn(f, s, (/x, y/), res=r)

  !
  ! Test 55
  !
  f = 'x+y'
  s = 'x[1.0 y'
  x = 1.0d0
  y = 1.0d0
  error='Not a correct error: [1.0'
  call s_checkfn(f, s, (/x, y/), perr=error)

  !
  ! Test 56
  !
  f = 'x+y'
  s = 'x y[1.3d0 ]'
  x = 1.0d0
  y = 1.0d0
  error='Not a correct error: [1.3d0'
  call s_checkfn(f, s, (/x, y/), perr=error)

  !
  ! Test 57
  !
  f = 'x+y'
  s = 'x[1] y'
  x = 10.0d0
  y = 20.0d0
  e = 1.0d0
  call s_checkfn(f, s, (/x, y/), reserr=e)

  !
  ! Test 58
  !
  f = 'x**2+y'
  s = 'x[1.0] y[1.5d0]'
  x = 10.0d0
  y = 20.0d0
  e = abs(2*x)*1.0d0+1.5d0
  call s_errprecision(e)
  call s_checkfn(f, s, (/x, y/), reserr=e)

  !
  ! Test 59
  !
  f = 'x**2+y**3'
  s = 'x[1.0] y[1.5d0]'
  x = 10.0d0
  y = 20.0d0
  e = abs(2*x)*1.0d0+abs(3*y**2)*1.5d0
  call s_errprecision(e)
  call s_checkfn(f, s, (/x, y/), reserr=e)

  !
  ! Test 60
  !
  f = 'sin(y)*x**2+y**3'
  s = 'x[0.2] y[0.1]'
  x = 1.0d0
  y = 2.0d0
  e = abs(2*x*sin(y))*0.2d0+abs(3*y**2)*0.1d0
  call s_errprecision(e)
  call s_checkfn(f, s, (/x, y/), reserr=e)

  !
  ! Test 61
  !
  f = 'sin(1/x)+cos(1/y)'
  s = 'x[0.01] y[0.01]'
  x = 10.0d0
  y = -20.0d0
  e = abs(-1.0d0/x**2*cos(1.0d0/x))*0.01d0+abs(1.0d0/y**2*sin(1.0d0/y))*0.01d0
  call s_errprecision(e)
  call s_checkfn(f, s, (/x, y/), reserr=e)

  !
  ! Test 62
  !
  f = 'sin(1/x)+cos(1/y)'
  s = 'x[0.01] y[0.01]'
  x = 0.1d0
  y = -0.2d0
  error = 'Error value not stable'
  call s_checkfn(f, s, (/x, y/), eeerr=error)

  !
  ! Test 63
  !
  f = 'sin(1/x)+cos(1/y)'
  s = 'x[0.05] y[0.05]'
  x = 0.1d0
  y = -0.2d0
  error = 'Error value not stable'
  call s_checkfn(f, s, (/x, y/), eeerr=error)

  !
  ! Test 64
  !
  f = 'sin(1/x)+cos(1/y)'
  s = 'x[0.0006] y[0.0005]'
  x = 0.1d0
  y = -0.2d0
  e = abs(-1.0d0/x**2*cos(1.0d0/x))*0.0006d0+abs(1.0d0/y**2*sin(1.0d0/y))*0.0005d0
  call s_errprecision(e)
  call s_checkfn(f, s, (/x, y/), reserr=e)

  !
  ! Test 65
  !
  f = '0**0'
  s = ''
  error = 'Undefined result 0**0'
  call s_checkfn(f, s, (/0.0d0/), eerr=error)

  !
  ! Test 66
  !
  f = 'step(x)'
  s = 'x[1.0d0]'
  x = 0.0d0
  error = 'Error value not stable'
  call s_checkfn(f, s, (/x/), eeerr=error)

  !
  ! Test 67
  !
  f = 'max(y, x)'
  s = 'x y'
  x = 1.0d0
  y = 2.0d0
  r = max(y, x)
  call s_checkfn(f, s, (/x, y/), res=r)

  !
  ! Test 68
  !
  f = 'max(x, y)'
  s = 'x y'
  x = 1.0d0
  y = 2.0d0
  r = max(x, y)
  call s_checkfn(f, s, (/x, y/), res=r)

  !
  ! Test 69
  !
  f = 'max(1+x, y)'
  s = 'x y'
  x = 1.0d0
  y = -2.0d0
  r = max(1+x, y)
  call s_checkfn(f, s, (/x, y/), res=r)

  !
  ! Test 70
  !
  f = 'max(x+y, x*(x-y))'
  s = 'x y'
  x = 1.0d0
  y = 2.0d0
  r = max(x+y, x*(x-y))
  call s_checkfn(f, s, (/x, y/), res=r)

  !
  ! Test 71
  !
  f = '3+max(x+y, x*(x-y))+4'
  s = 'x y'
  x = 1.0d0
  y = 2.0d0
  r = 3.0d0+max(x+y, x*(x-y))+4.0d0
  call s_checkfn(f, s, (/x, y/), res=r)

  !
  ! Test 72
  !
  f = '-1**4'
  s = ''
  r = -1.0d0**4.0d0
  call s_checkfn(f, s, (/0.0d0/), res=r)

  !
  ! Test 73
  !
  f = '(-1)**4.1'
  s = ''
  error = 'x**y, x<0 and y not integer'
  call s_checkfn(f, s, (/0.0d0/), eerr=error)

  !
  ! Test 74
  !
  f = '3+min(1.0d0+x+y, y*(y-x))**4'
  s = 'x y'
  x = 1.0d0
  y = 2.0d0
  r = 3.0d0+min(1.0d0+x+y, y*(y-x))**4.0d0
  call s_checkfn(f, s, (/x, y/), res=r)

  !
  ! Test 75
  !
  f = 'besj0(x)'
  s = 'x'
  x = 3.0d0
  r = f_besj0(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 76
  !
  f = 'besj1(x)'
  s = 'x'
  x = 3.0d0
  r = f_besj1(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 77
  !
  f = 'besjn(x, y)'
  s = 'x y'
  x = 3.0d0
  y = 4.0d0
  r = f_besjn(x, y)
  call s_checkfn(f, s, (/x, y/), res=r)

  !
  ! Test 78
  !
  f = 'besy0(x)'
  s = 'x'
  x = 3.0d0
  r = f_besy0(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 79
  !
  f = 'besy1(x)'
  s = 'x'
  x = 3.0d0
  r = f_besy1(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 80
  !
  f = 'besyn(x, y)'
  s = 'x y'
  x = 4.0d0
  y = 3.0d0
  r = f_besyn(x, y)
  call s_checkfn(f, s, (/x, y/), res=r)

  !
  ! Test 81
  !
  f = 'logn(x, y)'
  s = 'x y'
  x = 4.0d0
  y = 3.0d0
  r = log(y)/log(x)
  call s_checkfn(f, s, (/x, y/), res=r)

  !
  ! Test 82
  !
  f = 'logn(x, y)'
  s = 'x y'
  x = 0.0d0
  y = 3.0d0
  error = 'logn(x, y), x/>0 or y/>0'
  call s_checkfn(f, s, (/x, y/), eerr=error)

  !
  ! Test 83
  !
  f = 'logn(x, y)'
  s = 'x y'
  x = 4.0d0
  y = 0.0d0
  error = 'logn(x, y), x/>0 or y/>0'
  call s_checkfn(f, s, (/x, y/), eerr=error)

  !
  ! Test 84
  !
  f = 'erf(x)'
  s = 'x'
  x = 3.0d0
  r = f_erf(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 85
  !
  f = 'erfc(x)'
  s = 'x'
  x = 3.0d0
  r = 1.0d0 - f_erf(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 86
  !
  f = 'lgamma(x)'
  s = 'x'
  x = 3.0d0
  r = f_lgamma(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 87
  !
  f = 'gamma(x)'
  s = 'x'
  x = 3.1d0
  r = f_gamma(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 88
  !
  f = 'TAN(x)'
  s = 'x'
  x = 3.0d0
  r = tan(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 89
  !
  f = 'TaN(x)'
  s = 'x'
  x = 3.0d0
  r = tan(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 90
  !
  f = 'taN(x)'
  s = 'x'
  x = 3.0d0
  r = tan(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 91
  !
  f = '2*(3+2)'
  s = 'x'
  x = 3.0d0
  r = 2*(3+2)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 92
  !
  f = '2*(3+2'
  s = 'x'
  x = 4.0d0
  error = 'Unbalanced brackets: (3+2'
  call s_checkfn(f, s, (/x/), perr=error)

  !
  ! Test 93
  !
  f = 'TAN(1)'
  s = ' '
  r = tan(1.0d0)
  call s_checkfn(f, s, (/ 0.0d0 /), res=r)

  !
  ! Test 94
  !
  f = '(-2.0d0)**2.0d0'
  s = ' '
  r = 4.0d0
  call s_checkfn(f, s, (/ 0.0d0 /), res=r)

  !
  ! Test 95
  !
  f = '(-1)**(4.1)'
  s = ''
  error = 'x**y, x<0 and y not integer'
  call s_checkfn(f, s, (/0.0d0/), eerr=error)

  !
  !  Test 96
  !
  f = 'cos(x)+sin(y)+z**2'
  s = ' x  y   z '
  x = 1.1d0
  y = 2.2d0
  z = 3.3d0
  r = cos(x)+sin(y)+z**2
  call s_checkfn(f, s, (/x, y, z/), res=r)

  !
  !  Test 97
  !
  f = 'csch(x)'
  s = 'x'
  x = 1.1d0
  r = 1.0d0/sinh(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  !  Test 98
  !
  f = 'sech(x)'
  s = 'x'
  x = 1.1d0
  r = 1.0d0/cosh(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  !  Test 99
  !
  f = 'coth(x)'
  s = 'x'
  x = 1.1d0
  r = 1.0d0/tanh(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  !  Test 100
  !
  f = 'if(x<1, 100, 200)'
  s = 'x'
  x = 1.1d0
  r = 200.0d0
  call s_checkfn(f, s, (/x/), res=r)

  !
  !  Test 101
  !
  f = 'if(x>1, 100, 200)'
  s = 'x'
  x = 1.1d0
  r = 100.0d0
  call s_checkfn(f, s, (/x/), res=r)

  !
  !  Test 102
  !
  f = 'if(x=1, 100, 200)'
  s = 'x'
  x = 1.1d0
  r = 200.0d0
  call s_checkfn(f, s, (/x/), res=r)

  !
  !  Test 103
  !
  f = 'if(x=1, 100)'
  s = 'x'
  x = 1.1d0
  error = 'Incorrect components for: if('
  call s_checkfn(f, s, (/x/), perr=error)

  !
  !  Test 104
  !
  f = '1,2'
  s = ''
  error = 'Evaluation stack not empty'
  call s_checkfn(f, s, (/ 0.0d0 /), eerr=error)

  !
  !  Test 105
  !
  f = 'gauss(x)'
  s = 'x'
  x = 1.1d0
  r = exp(-x**2)
  call s_checkfn(f, s, (/x/), res=r)

  !
  !  Test 106
  !
  f = 'sinc(x)'
  s = 'x'
  x = 1.1d0
  r = sin(x)/x
  call s_checkfn(f, s, (/x/), res=r)

  !
  !  Test 107
  !
  f = 'sinc(x)'
  s = 'x'
  x = 0.0d0
  r = 1.0d0
  call s_checkfn(f, s, (/x/), res=r)

  !
  !  Test 108
  !
  f = 'x+1'
  s = 'x'
  x = 0.0d0
  error = 'OK'
  call s_checkfn(f, s, (/x/), perr=error)

  !
  !  Test 109
  !
  f = 'gamma(x)'
  s = 'x'
  x = 1.0d0/3.0d0
  r = 2.678938534708d0
  call s_checkfn(f, s, (/x/), res=r, tol=1.0d-11)

  !
  !  Test 110
  !
  f = 'gamma(x)'
  s = 'x'
  x = 0.5d0
  r = 1.772453850906d0
  call s_checkfn(f, s, (/x/), res=r, tol=1.0d-11)

  !
  !  Test 111
  !
  f = 'gamma(x)'
  s = 'x'
  x = -0.5d0
  r = -3.544907701811d0
  call s_checkfn(f, s, (/x/), res=r, tol=1.0d-5)

  !
  !  Test 112
  !
  f = 'gamma(x)'
  s = 'x'
  x = -1.5d0
  r = 2.363271801207d0
  call s_checkfn(f, s, (/x/), res=r, tol=1.0d-11)

  !
  !  Test 113
  !
  f = 'gamma(x)'
  s = 'x'
  x = 5.0d0
  r = 24.0d0
  call s_checkfn(f, s, (/x/), res=r)

  !
  !  Test 114
  !
  f = 'lgamma(x)'
  s = 'x'
  x = 0.5d0
  r = 0.5723649429d0
  call s_checkfn(f, s, (/x/), res=r, tol=1.0d-9)

  !
  !  Test 115
  !
  f = 'lgamma(x)'
  s = 'x'
  x = 2.5d0
  r = 0.2846828705d0
  call s_checkfn(f, s, (/x/), res=r, tol=1.0d-9)

  !
  !  Test 116
  !
  f = 'lgamma(x)'
  s = 'x'
  x = 5.0d0
  r = 3.178053830d0
  call s_checkfn(f, s, (/x/), res=r, tol=1.0d-9)

  !
  !  Test 117
  !
  f = 'lgamma(x)'
  s = 'x'
  x = 7.5d0
  r = 7.534364237d0
  call s_checkfn(f, s, (/x/), res=r, tol=1.0d-9)

  !
  !  Test 118
  !
  f = 'lgamma(x)'
  s = 'x'
  x = 10.0d0
  r = 12.80182748d0
  call s_checkfn(f, s, (/x/), res=r, tol=1.0d-9)

  !
  !  Test 119
  !
  f = 'lgamma(x)'
  s = 'x'
  x = -1.0d0
  error = 'lgamma(x), x<=0'
  call s_checkfn(f, s, (/x/), eerr=error)

  !
  !  Test 120
  !
  f = 'besj0(x)'
  s = 'x'
  x = 7.5d0
  r = 0.266339657880378d0
  call s_checkfn(f, s, (/x/), res=r, tol=1.0d-8)

  !
  !  Test 121
  !
  f = 'besj1(x)'
  s = 'x'
  x = 7.5d0
  r = 0.135248427579705d0
  call s_checkfn(f, s, (/x/), res=r, tol=1.0d-8)

  !
  !  Test 122
  !
  f = 'besjn(y, x)'
  s = 'x y'
  x = 7.5d0
  y = 3.0d0
  r = -0.258060913193460d0
  call s_checkfn(f, s, (/x, y/), res=r, tol=1.0d-8)

  !
  !  Test 123
  !
  f = 'besy0(x)'
  s = 'x'
  x = 7.5d0
  r = 0.117313286148209d0
  call s_checkfn(f, s, (/x/), res=r, tol=1.0d-8)

  !
  !  Test 124
  !
  f = 'besy1(x)'
  s = 'x'
  x = 7.5d0
  r = -0.259128510486116d0
  call s_checkfn(f, s, (/x/), res=r, tol=1.0d-8)

  !
  !  Test 125
  !
  f = 'besyn(y, x)'
  s = 'x y'
  x = 7.5d0
  y = 3.0d0
  r = 0.159707591937935d0
  call s_checkfn(f, s, (/x, y/), res=r, tol=1.0d-7)

  !
  !  Test 126
  !
  f = 'erf(x)'
  s = 'x'
  x = 0.5d0
  r = 0.520499877813047d0
  call s_checkfn(f, s, (/x/), res=r, tol=1.0d-8)

  !
  !  Test 127
  !
  f = 'erfc(x)'
  s = 'x'
  x = 0.5d0
  r = 0.479500122186953d0
  call s_checkfn(f, s, (/x/), res=r, tol=1.0d-8)

  !
  ! Test 128
  !
  f = '-1'
  s = ''
  r = -1.0d0
  error = 'OK'
  call s_checkfn(f, s, (/ 0.0d0 /), eerr=error)

  !
  ! Test 129
  !
  f = 'cos(x_y)+cos(x)+sin(y)+y_x**2'
  s = ' x  y   x_y  y_x'
  w = 0.5d0
  x = 1.1d0
  y = 2.2d0
  z = 3.3d0
  r = cos(y)+cos(w)+sin(x)+z**2
  call s_checkfn(f, s, (/w, x, y, z/), res=r, tol=1.0d-13)

  !
  ! Test 130
  !
  f = '(-1)/(-2)'
  s = ''
  r = 0.5d0
  error = 'OK'
  call s_checkfn(f, s, (/ 0.0d0 /), res=r)

  !
  ! Test 131
  !
  f = '(-1)/(2)'
  s = ''
  r = -0.5d0
  error = 'OK'
  call s_checkfn(f, s, (/ 0.0d0 /), res=r)

  !
  ! Test 132
  !
  f = '(1)/(2)'
  s = ''
  r = 0.5d0
  error = 'OK'
  call s_checkfn(f, s, (/ 0.0d0 /), res=r)

  !
  ! Test 133
  !
  f = '(1)/(-2)'
  s = ''
  r = -0.5d0
  error = 'OK'
  call s_checkfn(f, s, (/ 0.0d0 /), res=r)

  !
  ! Test 134
  !
  f = '-(1)/(2)'
  s = ''
  r = -0.5d0
  error = 'OK'
  call s_checkfn(f, s, (/ 0.0d0 /), res=r)

  !
  ! Test 135
  !
  f = 'if(x=1,10,100)'
  s = 'x'
  r = 10
  error = 'OK'
  call s_checkfn(f, s, (/ 1.0d0 /), eerr=error)

  !
  ! Test 136
  !
  f = 'besjn(1,-1)'
  s = ''
  r = -0.440050585677130123851696907877d0
  call s_checkfn(f, s, (/ 0.0d0 /), res=r, tol=1.0d-9)

  !
  ! Test 137
  !
  f = 'x_y+y'
  s = 'x_y y'
  x = 1.0d0
  y = 2.0d0
  r = 3.0d0
  call s_checkfn(f, s, (/ x, y /), res=r)

  !
  ! Test 138
  !
  f = 'x**0'
  s = 'x'
  x = 2.0d0
  r = 1.0d0
  call s_checkfn(f, s, (/ x /), res=r)

  !
  ! Test 139
  !
  f = '1e-01/-2e-01'
  s = 'x'
  error = "Operator following operator: -2e-01"
  call s_checkfn(f, s, (/ 0.0d0 /), perr=error)

  !
  ! Test 140
  !
  f = '1e-01/(-2e-01)'
  s = 'x'
  r = 1.0d-1/(-2.0d-1)
  call s_checkfn(f, s, (/ 0.0d0 /), res=r)

  !
  ! Test 141
  !
  f = 'a+1*2*sin(2*3)'
  s = 'a'
  error = 'OK'
  call s_checkfn(f, s, (/ 1.0d0 /), perr=error)

  !
  ! Test 142
  !
  f = 'a+3*a+a'
  s = 'a'
  error = 'OK'
  call s_checkfn(f, s, (/ 1.0d0 /), perr=error)

  !
  ! Test 143
  !
  f = '1e-01/*2e-01'
  s = 'x'
  error = "Operator following operator: *2e-01"
  call s_checkfn(f, s, (/ 0.0d0 /), perr=error)

  !
  ! Test 144
  !
  f = '1e-01+-2e-01'
  s = 'x'
  error = "Operator following operator: -2e-01"
  call s_checkfn(f, s, (/ 0.0d0 /), perr=error)

  !
  ! Test 145
  !
  f = 'K +4.0d0*K*CL'
  s = 'K CL'
  r = 1.0d0+4.0d0*1.0d0*1.0d0
  call s_checkfn(f, s, (/ 1.0d0, 1.0d0 /), res=r)

  !
  ! Test 146
  !
  f = 'K +(4.0d0*K*CL)'
  s = 'K CL'
  r = 1.0d0+4.0d0*1.0d0*1.0d0
  call s_checkfn(f, s, (/ 1.0d0, 1.0d0 /), res=r)

  !
  ! Test 147
  !
  f = 'K +(4.0d0*K*CLL)'
  s = 'K CL CLL'
  r = 1.0d0+4.0d0*1.0d0*2.0d0
  call s_checkfn(f, s, (/ 1.0d0, 1.0d0, 2.0d0 /), res=r)

  !
  ! Test 148
  !
  f = 'K +4.0d0*K*CLL'
  s = 'K CL CLL'
  r = 1.0d0+4.0d0*1.0d0*2.0d0
  call s_checkfn(f, s, (/ 1.0d0, 1.0d0, 2.0d0 /), res=r)

  !
  ! Test 149
  !
  f = '14.7231+9.6205688*0.05-90.9564642*0.05*0.05'
  s = 'a'
  r = 14.7231d0+9.6205688d0*0.05d0-90.9564642d0*0.05d0*0.05d0
  call s_checkfn(f, s, (/ 1.0d0 /), res=r)

  !
  ! Test 150
  !
  f = 'erf(x)'
  s = 'x'
  x = -3.0d0
  r = -0.9999779095030014d0
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 151
  !
  f = 'besi0(x)'
  s = 'x'
  x = 3.0d0
  r = f_besi0(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 152
  !
  f = 'besi1(x)'
  s = 'x'
  x = 3.0d0
  r = f_besi1(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 153
  !
  f = 'besin(x, y)'
  s = 'x y'
  x = 3.0d0
  y = 4.0d0
  r = f_besin(x, y)
  call s_checkfn(f, s, (/x, y/), res=r)

  !
  ! Test 154
  !
  f = 'besk0(x)'
  s = 'x'
  x = 3.0d0
  r = f_besk0(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 155
  !
  f = 'besk1(x)'
  s = 'x'
  x = 3.0d0
  r = f_besk1(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 156
  !
  f = 'beskn(x, y)'
  s = 'x y'
  x = 3.0d0
  y = 4.0d0
  r = f_beskn(x, y)
  call s_checkfn(f, s, (/x, y/), res=r)

  !
  ! Test 157
  !
  f = 'ierfc(x)'
  s = 'x'
  x = 0.5d0
  r = f_ierfc(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 158
  !
  f = 'ierfc(x)'
  s = 'x'
  x = 0.5d0
  r = 0.47693627620447d0
  call s_checkfn(f, s, (/x/), res=r, tol=1.0d-9)

  !
  ! Test 159
  !
  f = 'cbrt(x)'
  s = 'x'
  x = 3.0d0
  r = f_cbrt(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 160
  !
  f = 'besi0(x)'
  s = 'x'
  x = 1.0d0
  r = 1.266065878d0
  call s_checkfn(f, s, (/x/), res=r, tol=1.0d-9)

  !
  ! Test 161
  !
  f = 'besi1(x)'
  s = 'x'
  x = 1.0d0
  r = 0.5651591040d0
  call s_checkfn(f, s, (/x/), res=r, tol=1.0d-9)

  !
  ! Test 162
  !
  f = 'besin(x, y)'
  s = 'x y'
  x = 3.0d0
  y = 1.0d0
  r = 0.02216842492d0
  call s_checkfn(f, s, (/x, y/), res=r, tol=1.0d-9)

  !
  ! Test 163
  !
  f = 'besk0(x)'
  s = 'x'
  x = 1.0d0
  r = 0.4210244382d0
  call s_checkfn(f, s, (/x/), res=r, tol=1.0d-9)

  !
  ! Test 164
  !
  f = 'besk1(x)'
  s = 'x'
  x = 1.0d0
  r = 0.6019072302d0
  call s_checkfn(f, s, (/x/), res=r, tol=1.0d-9)

  !
  ! Test 165
  !
  f = 'beskn(x, y)'
  s = 'x y'
  x = 3.0d0
  y = 1.0d0
  r = 7.101262825d0
  call s_checkfn(f, s, (/x, y/), res=r, tol=1.0d-9)

  !
  ! Test 166
  !
  f = 'cbrt(x)'
  s = 'x'
  x = 3.0d0
  r = 1.442249570307d0
  call s_checkfn(f, s, (/x/), res=r, tol=1.0d-9)

  !
  ! Test 167
  !
  f = 'fresc(x)'
  s = 'x'
  x = 3.0d0
  r = f_fresc(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 168
  !
  f = 'fress(x)'
  s = 'x'
  x = 3.0d0
  r = f_fress(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 169
  !
  f = 'expi(x)'
  s = 'x'
  x = 3.0d0
  r = f_expi(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 170
  !
  f = 'sini(x)'
  s = 'x'
  x = 3.0d0
  r = f_sini(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 171
  !
  f = 'cosi(x)'
  s = 'x'
  x = 3.0d0
  r = f_cosi(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 172
  !
  f = 'logi(x)'
  s = 'x'
  x = 3.0d0
  r = f_logi(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 173
  !
  f = 'ierf(x)'
  s = 'x'
  x = 0.5d0
  r = f_ierf(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 174
  !
  f = 'elle(x)'
  s = 'x'
  x = 0.5
  r = f_elle(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 175
  !
  f = 'ellk(x)'
  s = 'x'
  x = 0.5d0
  r = f_ellk(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 176
  !
  f = 'ielle(x,y)'
  s = 'x y'
  x = 0.55d0
  y = 1.0d0
  r = f_ielle(x, y)
  call s_checkfn(f, s, (/x, y/), res=r)

  !
  ! Test 177
  !
  f = 'iellf(x,y)'
  s = 'x y'
  x = 0.55d0
  y = 1.0d0
  r = f_iellf(x, y)
  call s_checkfn(f, s, (/x, y/), res=r)

  !
  ! Test 178
  !
  f = 'fresc(x)'
  s = 'x'
  x = 3.0d0
  r = 0.6057207893d0
  call s_checkfn(f, s, (/x/), res=r, tol=1.0d-9)

  !
  ! Test 179
  !
  f = 'fress(x)'
  s = 'x'
  x = 3.0d0
  r = 0.4963129990d0
  call s_checkfn(f, s, (/x/), res=r, tol=1.0d-9)

  !
  ! Test 180
  !
  f = 'expi(x)'
  s = 'x'
  x = 3.0d0
  r = 9.933832571d0
  call s_checkfn(f, s, (/x/), res=r, tol=1.0d-9)

  !
  ! Test 181
  !
  f = 'sini(x)'
  s = 'x'
  x = 3.0d0
  r = 1.848652528d0
  call s_checkfn(f, s, (/x/), res=r, tol=1.0d-9)

  !
  ! Test 182
  !
  f = 'cosi(x)'
  s = 'x'
  x = 3.0d0
  r = 0.1196297860d0
  call s_checkfn(f, s, (/x/), res=r, tol=1.0d-9)

  !
  ! Test 183
  !
  f = 'logi(x)'
  s = 'x'
  x = 3.0d0
  r = 2.163588595d0
  call s_checkfn(f, s, (/x/), res=r, tol=1.0d-9)

  !
  ! Test 184
  !
  f = 'ierf(x)'
  s = 'x'
  x = 0.5d0
  r = 0.4769362762d0
  call s_checkfn(f, s, (/x/), res=r, tol=1.0d-9)

  !
  ! Test 185
  !
  f = 'elle(x)'
  s = 'x'
  x = 0.55
  r = 1.444243488d0
  call s_checkfn(f, s, (/x/), res=r, tol=1.0d-8)

  !
  ! Test 186
  !
  f = 'ellk(x)'
  s = 'x'
  x = 0.55d0
  r = 1.715354496d0
  call s_checkfn(f, s, (/x/), res=r, tol=1.0d-9)

  !
  ! Test 187
  !
  f = 'ielle(x,y)'
  s = 'x y'
  x = 0.55d0
  y = 1.0d0
  r = 0.9572122182d0
  call s_checkfn(f, s, (/x, y/), res=r, tol=1.0d-9)

  !
  ! Test 188
  !
  f = 'iellf(x,y)'
  s = 'x y'
  x = 0.55d0
  y = 1.0d0
  r = 1.046164709d0
  call s_checkfn(f, s, (/x, y/), res=r, tol=1.0d-9)

  !
  ! Test 189-198
  !
  f = 'ierf(x)'
  s = 'x'
  a(1:10) = (/ -1.163087154d0, -0.7328690780d0, -0.4769362762d0, &
       &-0.2724627147d0, -0.08885599049d0, 0.08885599049d0,&
       & 0.2724627147d0, 0.4769362762d0, 0.7328690780d0,&
       & 1.163087154d0 /)

  do i = 1, 10
     x = -1.1d0 + dble(i)/5.0d0
     call s_checkfn(f, s, (/x/), res=a(i), tol=1.0d-8)
  end do

  !
  ! Test 199-208
  !
  f = 'logi(x)'
  s = 'x'
  a(1:10) = (/-0.08512648673d0, -0.2529494192d0, -0.5468514142d0, &
       &-1.134011957d0, -inf, -0.9337872927d0, -0.1449910048d0,&
       & 0.3537475507d0, 0.7326370311d0, 1.0451637801d0 /)

  do i = 1, 10
     x = dble(i)/5.0d0
     call s_checkfn(f, s, (/x/), res=a(i), tol=1.0d-9)
  end do

  !
  ! Test 209-218
  !
  f = 'expi(x)'
  s = 'x'
  a(1:10) = (/-0.00003766562284d0, -0.0003600824522d0, &
       &-0.003779352410d0, -0.04890051071d0, -inf, 4.954234356d0,&
       & 19.63087447d0, 85.98976214d0, 440.3798995d0, 2492.228976d0 /)

  do i = 1, 10
     x = -10.0d0 + i*2.0d0
     call s_checkfn(f, s, (/x/), res=a(i), tol=1.0d-9)
  end do

  !
  ! Test 219
  !
  f = 'expi(x)'
  s = 'x'
  x = 45.0d0
  r = 7.943916036d17
  call s_checkfn(f, s, (/x/), res=r, tol=1.0d-9)

  !
  ! Test 220
  !
  f = 'expi(x)'
  s = 'x'
  x = -45.0d0
  r = -6.225690809d-22
  call s_checkfn(f, s, (/x/), res=r, tol=1.0d-9)

  !
  ! Test 221
  !
  f = 'if(x<y, if(z>=y, 1.0, 2.0), 0.0)'
  s = 'x y z'
  x = 2
  y = 1
  z = 3
  call s_checkfn(f, s, (/x, y, z/), perr='OK')

  !
  ! Test 222
  !
  f = 'if(x==y,z+1,z*(2**(-abs(x-y))))'
  s = 'x y z'
  x = 1
  y = 2
  z = 3
  call s_checkfn(f, s, (/ x, y, z/), perr='OK')

  !
  ! Test 223
  !
  f = 'besjn(x, besjn(y, z))'
  s = 'x y z'
  x = 1
  y = 2
  z = 3
  call s_checkfn(f, s, (/ x, y, z/), perr='OK')

  !
  ! Test 224
  !
  f = 'if(x>-1,1,2)'
  s = 'x'
  x = 1
  call s_checkfn(f, s, (/ x /), perr='OK')

  !
  ! Test 225
  !
  f = 'x>-1'
  s = 'x'
  x = 1
  call s_checkfn(f, s, (/ x /), perr='OK')

  !
  ! Test 226
  !
  f = '-1'
  s = 'x'
  x = 1
  call s_checkfn(f, s, (/ x /), perr='OK')

  !
  ! Test 227
  !
  f = '+1'
  s = 'x'
  x = 1
  call s_checkfn(f, s, (/ x /), perr='OK')

  !
  ! Test 228
  !
  f = 'x+-1'
  s = 'x'
  x = 1
  call s_checkfn(f, s, (/ x /), perr='Operator following operator: -1')

  !
  ! Test 229
  !
  f = 'x+-1'
  s = 'x'
  x = 2.0d0
  call s_checkfn(f, s, (/ x /), perr='Operator following operator: -1')

  !
  ! Test 230
  !
  f = 'x--1'
  s = 'x'
  x = 2.0d0
  call s_checkfn(f, s, (/ x /), perr='Operator following operator: -1')

  !
  ! Test 231
  !
  f = 'x-+1'
  s = 'x'
  x = 2.0d0
  call s_checkfn(f, s, (/ x /), perr='Operator following operator: +1')

  !
  ! Test 232
  !
  f = 'x++1'
  s = 'x'
  x = 2.0d0
  call s_checkfn(f, s, (/ x /), perr='Operator following operator: +1')

  !
  ! Test 233
  !
  f = 'x>2'
  s = 'x'
  x = 1
  call s_checkfn(f, s, (/ x /), res=0.0d0)

  !
  ! Test 234
  !
  f = 'x<2'
  s = 'x'
  x = 1
  call s_checkfn(f, s, (/ x /), res=1.0d0)

  !
  ! Test 235
  !
  f = 'x>-1'
  s = 'x'
  x = 1
  call s_checkfn(f, s, (/ x /), res=1.0d0)

  !
  ! Test 236
  !
  f = 'x<-1'
  s = 'x'
  x = 1
  call s_checkfn(f, s, (/ x /), res=0.0d0)

  !
  ! Test 237
  !
  f = 'if(x>-1,1,2)'
  s = 'x'
  x = 1
  call s_checkfn(f, s, (/ x /), res=1.0d0)

  !
  ! Test 238
  !
  f = '0.5d0 - 0.5d0*cos(x-2.0d0*y)'
  s = 'x y'
  x = 499.999d0
  y = 99.9998d0
  r = 0.5d0 - 0.5d0*cos(x-2.0d0*y)
  call s_checkfn(f, s, (/ x, y /), res=r)

  !
  ! Test 239
  !
  f = 'MAC+1'
  s = 'MAC'
  x = 1.0d0
  r = 2.0d0
  call s_checkfn(f, s, (/ x /), res=r)

  !
  ! Test 240
  !
  f = 'x**2+sqrt(x)/x'
  s = 'x'
  x = 2.0d0
  r = x**2+sqrt(x)/x
  call s_checkfn(f, s, (/ x /), res=r)

  !
  ! Test 241
  !
  f = 'x%y'
  s = 'x y'
  x = 11.0d0
  y = 5.0d0
  r = 1.0d0
  call s_checkfn(f, s, (/ x, y /), res=r)

  !
  ! Test 242
  !
  f = 'modulo(x,y)'
  s = 'x y'
  x = 11.0d0
  y = 5.0d0
  r = 1.0d0
  call s_checkfn(f, s, (/ x, y /), res=r)
  
  !
  ! Test 243
  !
  f = 'x%y'
  s = 'x y'
  x = 11.1d0
  y = 5.9d0
  error='x%y, x not int'
  call s_checkfn(f, s, (/ x, y /), eerr=error)

  !
  ! Test 244
  !
  f = 'modulo(x,y)'
  s = 'x y'
  x = 11.1d0
  y = 5.9d0
  r = 5.2d0
  call s_checkfn(f, s, (/ x, y /), res=r)
  
  !
  ! Test 245
  !
  f = 'modulo(x,y)'
  s = 'x y'
  x = 11.0d0
  y = 5.5d0
  r = 0.0d0
  call s_checkfn(f, s, (/ x, y /), res=r)
  
  !
  ! Test 246
  !
  f = 'x%y'
  s = 'x y'
  x = 11.0d0
  y = 5.0d0
  r = 1.0d0
  call s_checkfn(f, s, (/ x, y /), res=r)

  !
  ! Test 247
  !
  f = 'ceiling(x)'
  s = 'x'
  x = 5.2d0
  r = 6.0d0
  call s_checkfn(f, s, (/ x /), res=r)

  !
  ! Test 248
  !
  f = 'floor(x)'
  s = 'x'
  x = 5.2d0
  r = 5.0d0
  call s_checkfn(f, s, (/ x /), res=r)
  
  !
  ! Test 249
  !
  f = 'mod(x,y)'
  s = 'x y'
  x = 11.1d0
  y = 5.9d0
  r = mod(x,y)
  call s_checkfn(f, s, (/ x, y /), res=r)

  !
  ! Test 250
  !
  f = 'x%y'
  s = 'x y'
  x = 11.0d0
  y = 5.9d0
  error='x%y, y not int'
  call s_checkfn(f, s, (/ x, y /), eerr=error)

  !
  ! Test 251
  !
  f = 'x%y'
  s = 'x y'
  x = 11.0d0
  y = 0.0d0
  error='x%y, y=0'
  call s_checkfn(f, s, (/ x, y /), eerr=error)

  !
  ! Test 252
  !
  f = 'x,y'
  s = 'x y'
  x = 11.0d0
  y = 0.0d0
  error = "Evaluation stack not empty"
  call s_checkfn(f, s, (/ x, y /), eerr=error)

  !
  ! Test 253
  !
  f = 'x//y'
  s = 'x y'
  x = 3
  y = 4
  r = 0
  call s_checkfn(f, s, (/ x, y /), res=r)

  !
  ! Test 254
  !
  f = 'x//y'
  s = 'x y'
  x = 4
  y = 3
  r = 1
  call s_checkfn(f, s, (/ x, y /), res=r)

  !
  ! Test 255
  !
  f = 'x//y'
  s = 'x y'
  x = 13
  y = 4
  r = 3
  call s_checkfn(f, s, (/ x, y /), res=r)

  !
  ! Test 255
  !
  f = 'x//y'
  s = 'x y'
  x = 13.1d0
  y = 4.1d0
  error = 'x//y, x not int'
  call s_checkfn(f, s, (/ x, y /), eerr=error)

  !
  ! Test 255
  !
  f = 'x//y'
  s = 'x y'
  x = 13
  y = 4.1d0
  error = 'x//y, y not int'
  call s_checkfn(f, s, (/ x, y /), eerr=error)

end program check