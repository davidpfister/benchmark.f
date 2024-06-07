module params
    implicit none
  
    !  Useful constants
    integer, parameter :: &
         fdp=8, variablelength=256, errorlength=256
  
    real(fdp), parameter :: pi = 3.141592653589793238d0, pi2 = pi/2.0d0, &
         pi4 = pi/4.0d0, eps = 1.0d-16, inf = 1.7976931348623157d308, &
         el=0.5772156649015329d0, sqrt2 = 1.414213562373095d0, zero=2.2250738585072014d-308
  
  
    character(len=*), parameter :: &
         operatorchars='+-/*^><=!%', numberchars='1234567890.', &
         exponentchars='ed', lowerchars='abcdefghijklmnopqrstuvwxyz', &
         upperchars='ABCDEFGHIJKLMNOPQRSTUVWXYZ', bracketchars='()[]{}', &
         specialchars=',_', funcvarchars=lowerchars//upperchars, &
         allowedchars=operatorchars//numberchars//funcvarchars//bracketchars//specialchars
  
    ! Controlling definitions
    integer, parameter :: &
         number_=10, bracket_=11, nop_=12, comma_=13, end_=14
  
    !  Bracket definitions
    integer, parameter :: &
         bracketstart=20, &
         roundbracket_=20, squarebracket_=21, squigbracket_=22, &
         bracketend=22
  
    !  Expression definitions
    integer, parameter :: &
         expressionlength=8, &
         expressionstart=100, &
         anint_=100, log10_=101, sqrt_=102, acos_=103, asin_=104, &
         atan_=105, cosh_=106, sinh_=107, tanh_=108, aint_=109, &
         cos_=110, sin_=111, tan_=112, exp_=113, log_=114, abs_=115, &
         delta_=116, step_=117, hat_=118, max_=119, min_=120, &
         besj0_=121, besj1_=122, besjn_=123, besy0_=124, besy1_=125, &
         besyn_=126, logn_=127, erf_=128, erfc_=129, lgamma_=130, &
         gamma_=131, csch_=132, sech_=133, coth_=134, if_=135, &
         gauss_=136, sinc_=137, besi0_=138, besi1_=139, besk0_=140, &
         besk1_=141, ierfc_=142, besin_=143, beskn_=144, cbrt_=145, &
         fresc_=146, fress_=147, expi_=148, sini_=149, cosi_=150, &
         logi_=151, ierf_=152, elle_=153, ellk_=154, ielle_=155, iellf_=156, &
         modulo_=157 , floor_=158, ceiling_=159, mod_=160, &
         expressionend=160
  
    character(len=expressionlength), dimension(expressionstart:expressionend), parameter :: &
         expressions=(/ 'anint(  ', 'log10(  ', 'sqrt(   ', 'acos(   ', &
         'asin(   ', 'atan(   ', 'cosh(   ', 'sinh(   ', 'tanh(   ', 'aint(   ', &
         'cos(    ', 'sin(    ', 'tan(    ', 'exp(    ', 'log(    ', 'abs(    ', &
         'delta(  ', 'step(   ', 'hat(    ', 'max(    ', 'min(    ', 'besj0(  ', &
         'besj1(  ', 'besjn(  ', 'besy0(  ', 'besy1(  ', 'besyn(  ', 'logn(   ', &
         'erf(    ', 'erfc(   ', 'lgamma( ', 'gamma(  ', 'csch(   ', 'sech(   ', &
         'coth(   ', 'if(     ', 'gauss(  ', 'sinc(   ', 'besi0(  ', 'besi1(  ', &
         'besk0(  ', 'besk1(  ', 'ierfc(  ', 'besin(  ', 'beskn(  ', 'cbrt(   ', &
         'fresc(  ', 'fress(  ', 'expi(   ', 'sini(   ', 'cosi(   ', 'logi(   ', &
         'ierf(   ', 'elle(   ', 'ellk(   ', 'ielle(  ', 'iellf(  ', 'modulo( ', &
         'floor(  ', 'ceiling(', 'mod(    '  &
         /)
        
  
    integer, dimension(expressionstart:expressionend), parameter :: &
         commandcomps=(/ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, &
         1, 1, 1, 1, 1, 2, 2, 1, 1, 2, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, &
         1, 3, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, &
         1, 1, 1, 2, 2, 2, 1, 1, 2 /)
    integer, parameter :: &
         maxcommandcomps=4
    integer, dimension(expressionstart:expressionend) :: expressionslen=len_trim(expressions)
  
    !  Operators definitions
    integer, parameter :: &
         operatorlength=2, &
         operatorstart=500, &
         conditionalstart=500, &
         neq_=500, eqeq_=501, gteq_=502, eqgt_=503, lteq_=504, eqlt_=505, eq_=506, gt_=507, lt_=508,&
         conditionalend=508, &
         opsstart=509, &
         pow_=509, caret_=510, mult_=511, idiv_=512, div_=513, perc_=514, &
         opsend=514, &
         pmstart=515, &
         plus_=515, minus_=516, &
         pmend=516, &
         operatorend=516
  
    character(len=operatorlength), dimension(operatorstart:operatorend), parameter :: &
         operators=(/ '!=', '==', '>=', '=>', '<=', '=<', '= ', &
         '> ', '< ', '**', '^ ', '* ', '//', '/ ', '% ', '+ ', '- ' /)
  
    integer, dimension(operatorstart:operatorend), parameter :: &
         operatorsorder=(/ 10, 10, 10, 10, 10, 10, 10, 10, 10, &
         10000, 10000, 1000, 1000, 1000, 1000, 100, 100 /)
    integer, dimension(operatorstart:operatorend) :: operatorslen=len_trim(operators)
  
    !  Variable definitions
    integer, parameter :: &
         variablestart=1000, &
         variableend=9999
  
    ! Separates logical blocks of code
    integer, dimension(10), parameter :: blockseparator = (/ comma_, neq_, eqeq_, gteq_, &
         eqgt_, lteq_, eqlt_, eq_, gt_, lt_ /) 
  
  end module params