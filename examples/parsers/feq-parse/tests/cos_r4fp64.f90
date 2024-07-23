
integer function cos_r4fp64() result(r)
  use FEQParse
  use iso_fortran_env
  implicit none
  real(real64),parameter :: pi = 4.0_real64*atan(1.0_real64)
  integer,parameter :: N = 2
  integer,parameter :: M = 5
  type(EquationParser) :: f
  character(LEN=1),dimension(1:3) :: independentVars
  character(LEN=1024) :: eqChar
  real(real64),allocatable :: x(:,:,:,:,:)
  real(real64),allocatable :: feval(:,:,:,:)
  real(real64),allocatable :: fexact(:,:,:,:)
  integer :: i,j,k,l

  allocate (x(1:N,1:N,1:N,1:M,1:3), &
            feval(1:N,1:N,1:N,1:M), &
            fexact(1:N,1:N,1:N,1:M))

  ! Specify the independent variables
  independentVars = (/'x','y','z'/)

  ! Specify an equation string that we want to evaluate
  eqChar = 'f = cos( 2.0*pi*x )*cos( 2.0*pi*y )*cos( 2.0*pi*z )'

  ! Create the EquationParser object
  f = EquationParser(eqChar,independentVars)

  x = 0.0_real64
  do l = 1,M
    do k = 1,N
    do j = 1,N
      do i = 1,N
        x(i,j,k,l,1) = -1.0_real64 + (2.0_real64)/real(N,real64)*real(i - 1,real64) + 2.0_real64*real(l - 1,real64)
        x(i,j,k,l,2) = -1.0_real64 + (2.0_real64)/real(N,real64)*real(j - 1,real64)
        x(i,j,k,l,3) = -1.0_real64 + (2.0_real64)/real(N,real64)*real(k - 1,real64)
      end do
    end do
    end do
  end do
  do l = 1,M
    do k = 1,N
    do j = 1,N
      do i = 1,N
       fexact(i,j,k,l) = cos(2.0_real64*pi*x(i,j,k,l,1))*cos(2.0_real64*pi*x(i,j,k,l,2))*cos(2.0_real64*pi*x(i,j,k,l,3))
      end do
    end do
    end do
  end do

  ! Evaluate the equation
  feval = f % evaluate(x)
  if (maxval(abs(feval - fexact)) <= epsilon(1.0_real64)) then
    r = 0
  else
    r = 1
  end if

  deallocate (x,feval,fexact)

end function cos_r4fp64