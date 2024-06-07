module interpreter
   use params
   use extrafunc
 
   implicit none
 
   private
   public :: s_createfn, s_errprecision, s_evaluatefn, s_evaluateerr, s_destroyfn
 
   type cachefn
      real(fdp), dimension(:), pointer :: numbers
      integer, dimension(:), pointer :: commands
   end type cachefn
 
   character(len=variablelength), dimension(:), allocatable :: variables
   integer, dimension(:), allocatable :: lenvariables
   integer :: numvariables
   real(fdp), dimension(:), allocatable :: varerrors
   character(len=errorlength) :: error
   type(cachefn), dimension(:), pointer, save :: cachedfuncs
 
 contains
 
   subroutine s_firstrun()
     logical, save :: firstrun = .true.
     integer :: i
 
     if (firstrun) then
        allocate(cachedfuncs(10))
 
        do i=1, 10
           nullify(cachedfuncs(i)%numbers, cachedfuncs(i)%commands)
        end do
 
        firstrun = .false.
     end if
   end subroutine s_firstrun
 
 
   
   subroutine s_seterror(t_error)
     character(len=*), intent(in) :: t_error
 
     if (error == 'OK' .and. len_trim(t_error) > 0) error = trim(t_error)
   end subroutine s_seterror
 
 
 
   function f_clean(func) result(tmpfunc)
     character(len=*), intent(in) :: func
     character(len=len(func)) :: tmpfunc
     integer :: i, c, l
     
     tmpfunc = adjustl(func)
     l = len_trim(tmpfunc)
     i = 1
 
     do while (i <= l)
        if (tmpfunc(i:i)== ' ') then
           tmpfunc(i:) = tmpfunc(i+1:)
           l = l - 1
        else
           c = ichar(tmpfunc(i:i))
           if ((c <= ichar('Z')) .and. (c >= ichar('A'))) tmpfunc(i:i) = achar(c-ichar('A')+ichar('a'))
           i = i + 1
        end if
     end do
   end function f_clean
 
 
 
   function f_numbervars(vars) result(numvars)
     character(len=*), intent(in) :: vars
     integer :: numvars
     character(len=len(vars)) :: tmpvars
     character(len=variablelength) :: tmpvar
 
     tmpvars = trim(adjustl(vars))
     numvars = 0
 
     do while (len_trim(tmpvars) > 0)
        read(tmpvars, *) tmpvar
        numvars = numvars + 1
        tmpvars = tmpvars(index(tmpvars, trim(tmpvar))+len_trim(tmpvar):)
     end do
   end function f_numbervars
 
 
 
   subroutine s_errprecision(number)
     real(fdp), intent(inout) :: number
     real(fdp) :: tmp1, tmp2
 
     !
     ! Format the error to the correct precision
     !
 
     if (number /= 0.0d0) then
        tmp1 = 10.0d0**(floor(log10(number)))
        tmp2 = anint(number/tmp1)
 
        if (tmp2 > 1.0d0) then
           number = tmp2*tmp1
        else
           tmp1 = 10.0d0**(floor(log10(number))-1.0d0)
           tmp2 = anint(number/tmp1)
           number = tmp2*tmp1
        end if
     end if
   end subroutine s_errprecision
 
 
 
   subroutine s_readvarerror(errorstring, errorval)
     character(len=*), intent(in) :: errorstring
     real(fdp) :: errorval
     integer :: command, lenerror
     character(len=errorlength) :: tmperror
 
     errorval = 0.0d0
     lenerror = len_trim(errorstring)
     command = nop_
 
     if (errorstring(1:1) == '[' .and. errorstring(lenerror:lenerror) == ']') then
        tmperror = errorstring(2:lenerror-1)
        
        call s_readnumber(tmperror, lenerror, command, errorval)
 
        if (command /= number_) call s_seterror('Not a number: '//tmperror)
 
        if (len_trim(tmperror) /= 0) call s_seterror('Not correct error format: '//tmperror)
     else
        call s_seterror('Not a correct error: '//errorstring)
     end if
   end subroutine s_readvarerror
 
 
 
   subroutine s_allocatevariables(vars)
     character(len=*), intent(in) :: vars
     integer :: numvars, i, bracketpos, j, c
     character(len=variablelength+errorlength), dimension(:), allocatable :: tmp
 
     numvars = f_numbervars(vars)
 
     allocate(tmp(numvars), variables(numvars), lenvariables(numvars), varerrors(numvars))
 
     read(vars, *) tmp
 
     do i = 1, numvars
        do j = 1, len_trim(tmp(i))
           c = ichar(tmp(i)(j:j))
           if ((c <= ichar('Z')) .and. (c >= ichar('A'))) tmp(i)(j:j) = achar(c-ichar('A')+ichar('a'))
        end do
 
        bracketpos = scan(tmp(i), "[")
 
        if (bracketpos /= 0) then
           variables(i) = trim(tmp(i)(:bracketpos-1))
 
           call s_readvarerror(trim(tmp(i)(bracketpos:)), varerrors(i))
        else
           variables(i) = trim(tmp(i))
           varerrors(i) = 0.0d0
        end if
     end do
 
     lenvariables = len_trim(variables)
     numvariables = numvars
 
     deallocate(tmp)
   end subroutine s_allocatevariables
 
 
 
   subroutine s_deallocatevariables()
     deallocate(variables, lenvariables, varerrors)
   end subroutine s_deallocatevariables
 
 
 
   subroutine s_funcpush(funcstack, funcstackn, command, number)
     type(cachefn), intent(inout) :: funcstack
     integer, intent(inout) :: funcstackn
     real(fdp), intent(in) :: number
     integer, intent(in) :: command
     type(cachefn) :: tmpstack
     integer :: n

     funcstackn = funcstackn + 1
     n = size(funcstack%numbers)

      if (funcstackn > n-2) then
        allocate(tmpstack%numbers(funcstackn+10), tmpstack%commands(funcstackn+10))
        tmpstack%numbers(:n) = funcstack%numbers(:n)
        tmpstack%commands(:n) = funcstack%commands(:n)
        tmpstack%numbers(n+1:) = 0.0d0
        tmpstack%commands(n+1:) = end_
        deallocate(funcstack%numbers, funcstack%commands)
        funcstack%numbers => tmpstack%numbers
        funcstack%commands => tmpstack%commands
     end if
 
     funcstack%commands(funcstackn) = command
     funcstack%numbers(funcstackn) = number
   end subroutine s_funcpush
 
 
 
   subroutine s_checkoperations(func, lenfunc, command, nextcommand)
     integer, intent(in) :: lenfunc
     character(lenfunc), intent(in) :: func
     integer, intent(in) :: command, nextcommand
 
     select case(command)
 
     case(number_)
        select case(nextcommand)
 
        case(number_)
           call s_seterror('Number following number: '//func)
 
        case(variablestart:variableend)
           call s_seterror('Variable following number: '//func)
 
        case(bracket_)
           call s_seterror('Bracket following number: '//func)
 
        case(expressionstart:expressionend)
           call s_seterror('Expression following number: '//func)
 
        end select
        
     case(variablestart:variableend)
        select case(nextcommand)
           
        case(number_)
           call s_seterror('Number following variable: '//func)
           
        case(variablestart:variableend)
           call s_seterror('Variable following variable: '//func)
           
        case(bracket_)
           call s_seterror('Bracket following variable: '//func)
           
        case(expressionstart:expressionend)
           call s_seterror('Expression following variable: '//func)
           
        end select
        
     case(conditionalstart:conditionalend)
        select case(nextcommand)
 
        case(conditionalstart:conditionalend, opsstart:opsend)
           call s_seterror('Operator following operator: '//func)
 
        case(end_)
           call s_seterror('Premature end of function')
 
        end select
 
     case(opsstart:opsend, pmstart:pmend)
        select case(nextcommand)
 
        case(operatorstart:operatorend)
           call s_seterror('Operator following operator: '//func)
 
        case(end_)
           call s_seterror('Premature end of function')
 
        end select
     end select
   end subroutine s_checkoperations
 
 
 
   subroutine s_checkcomponents(components, command)
     integer, intent(in) :: components, command
     
     if (components /= commandcomps(command)) call s_seterror('Incorrect components for: '//expressions(command))
   end subroutine s_checkcomponents
 
 
 
   subroutine s_readnumber(func, lenfunc, command, number)
     integer, intent(inout) :: lenfunc
     character(lenfunc), intent(inout) :: func
     integer, intent(out) :: command
     real(fdp), intent(out) :: number
     integer :: pos
     logical :: dflag, exponent, finish
 
     exponent = .false.
     dflag = .false.
     finish = .false.
     pos = 0
     number = 0.0d0
     command = nop_
 
     do while(pos < lenfunc .and. .not. finish)
        pos = pos + 1
 
        select case(func(pos:pos))
 
        case ('0':'9')
           if (dflag) then
              exponent = .true.
              dflag = .false.
           end if
 
        case ('d','e','D','E')
           if (dflag .or. exponent) then
              finish = .true.
           else
              dflag = .true.
           end if
 
        case('.')
           if (dflag .or. exponent) finish = .true.
 
        case('+', '-')
           if (dflag) then
              dflag = .false.
              exponent = .true.
           else
              finish = .true.
           end if
 
        case default
           finish = .true.
 
        end select
     end do
 
     if (pos == lenfunc .and. .not. finish) then
        read(func, *) number
 
        command = number_
 
        func = ''
        lenfunc = 0
     else
        read(func(:pos-1), *) number
 
        command = number_
 
        func = func(pos:)
        lenfunc = lenfunc - pos + 1
     end if
 
   end subroutine s_readnumber
 
 
 
   subroutine s_readtext(func, lenfunc, command, number)
     integer, intent(inout) :: lenfunc
     character(lenfunc), intent(inout) :: func
     integer, intent(out) :: command
     real(fdp), intent(out) :: number
     integer :: i, funccut
 
     funccut = 0
     command = nop_
     number = 0.0d0
 
     do i = expressionstart, expressionend
        if (expressionslen(i) < lenfunc) then
           if ((func(:expressionslen(i)) == expressions(i)(:expressionslen(i))) &
                .and. expressionslen(i) > funccut ) then
              command = i
              funccut = expressionslen(i)-1
           end if
        end if
     end do
 
     if (command == nop_) then
        do i = 1, numvariables
           if (func(:min(lenvariables(i), lenfunc)) == variables(i) .and. &
                &lenvariables(i) > funccut) then
              command = variablestart+i
              number = varerrors(i)
              funccut = lenvariables(i)
           end if
        end do
     end if
 
     if (command == nop_) then
        call s_seterror('Unknown text: '//func)
 
        func = ''
        lenfunc = 0
     end if
     
     if (lenfunc > funccut) then
        func = func(funccut+1:)
        lenfunc = lenfunc - funccut
     else
        func = ''
        lenfunc = 0
     end if
   end subroutine s_readtext
 
 
 
   subroutine s_readoperator(func, lenfunc, command, order)
     integer, intent(inout) :: lenfunc
     character(lenfunc), intent(inout) :: func
     integer, intent(out) :: command, order
     integer :: i, funccut
 
     funccut = 0
     command = nop_
     order = huge(order)
 
     do i = operatorstart, operatorend
        if (func(:min(operatorslen(i), lenfunc)) == operators(i) .and. &
             &operatorslen(i) > funccut ) then
           command = i
           order = operatorsorder(i)
           funccut = operatorslen(i)
        end if
     end do
 
     if (command == nop_) then
        call s_seterror('Unknown operator: '//func)
 
        func = ''
        lenfunc = 0
     end if
 
     if (lenfunc > funccut) then
        func = func(funccut+1:)
        lenfunc = lenfunc - funccut
     else
        func = ''
        lenfunc = 0
     end if
   end subroutine s_readoperator
 
 
 
   subroutine s_readbracket(func, lenfunc, subfunc, lensubfunc)
     integer, intent(inout) :: lenfunc
     character(lenfunc), intent(inout) :: func
     character(len=len(func)), intent(out) :: subfunc
     integer, intent(out) :: lensubfunc
     integer :: pos, brackettype
     integer, dimension(bracketstart:bracketend) :: count
 
     count = 0
     brackettype = 0
     subfunc = ''
     
     select case(func(1:1))
        
     case('(')
        brackettype = roundbracket_
        
     case('[')
        brackettype = squarebracket_
        
     case('{')
        brackettype = squigbracket_
        
     end select
 
     do pos = 1, lenfunc
        select case(func(pos:pos))
           
        case('(')
           count(roundbracket_) = count(roundbracket_) + 1
           
        case(')')
           count(roundbracket_) = count(roundbracket_) - 1
           
        case('[')
           count(squarebracket_) = count(squarebracket_) + 1
           
        case(']')
           count(squarebracket_) = count(squarebracket_) - 1
           
        case('{')
           count(squigbracket_) = count(squigbracket_) + 1
           
        case('}')
           count(squigbracket_) = count(squigbracket_) - 1
 
        end select
        
        if (count(brackettype) == 0) then
           exit
        end if
     end do
     
     subfunc = func(2:pos-1)
     lensubfunc = pos - 2
 
     if (sum(abs(count)) /= 0) then
        call s_seterror('Unbalanced brackets: '//func)
        
        func = ''
        lenfunc = 0
     end if
     
     if (lensubfunc == 0) then
        call s_seterror('Empty brackets encountered: '//func)
        
        func = ''
        lenfunc = 0
     end if
     
     if (lenfunc < pos+1) then
        func = ''
        lenfunc = 0
     else
        func = func(pos+1:)
        lenfunc = lenfunc - pos
     end if
   end subroutine s_readbracket
 
 
 
   subroutine s_readcomma(func, lenfunc, command, order)
     integer, intent(inout) :: lenfunc
     character(lenfunc), intent(inout) :: func
     integer, intent(out) :: command, order
     
     command = comma_
     order = 0
     
     if (lenfunc < 2) then
        func = ''
        lenfunc = 0
     else
        func = func(2:)
        lenfunc = lenfunc - 1
     end if
   end subroutine s_readcomma
 
 
 
   recursive subroutine s_parseatom(func, lenfunc, funcstack, funcstackn, command, number, order, components)
     type(cachefn), intent(inout) :: funcstack
     integer, intent(inout) :: funcstackn
     integer, intent(inout) :: lenfunc, components
     character(lenfunc), intent(inout) :: func
     integer, intent(in) :: command, order
     real(fdp), intent(in) :: number
     character(len=len(func)) :: tmpfunc
     integer :: lentmpfunc, nextcommand, nextorder, newcomponents
     real(fdp) :: nextnumber
 
     select case(command)
 
     case(number_, variablestart:variableend)
        call s_funcpush(funcstack, funcstackn, command, number)
 
     case(nop_, end_)
 
     case(comma_)
        call s_parseblock(command, func, lenfunc, funcstack, funcstackn, order, components)
        
        components = components + 1
        
     case(bracket_)
        call s_readbracket(func, lenfunc, tmpfunc, lentmpfunc)
        call s_parsefn(tmpfunc, lentmpfunc, funcstack, funcstackn, components)
 
     case(operatorstart:operatorend)
        call s_parseblock(command, func, lenfunc, funcstack, funcstackn, order, components)
        call s_funcpush(funcstack, funcstackn, command, number)
 
     case(expressionstart:expressionend)
        call s_getnextatom(func, lenfunc, nextcommand, nextnumber, nextorder)
        call s_checkoperations(func, lenfunc, command, nextcommand)
 
        newcomponents = 1
        call s_parseatom(func, lenfunc, funcstack, funcstackn, nextcommand, nextnumber, nextorder, newcomponents)
        call s_checkcomponents(newcomponents, command)
        call s_funcpush(funcstack, funcstackn, command, number)
        
     case default
        call s_seterror('Unknown input: '//func)
        
        func = ''
        lenfunc = 0
 
     end select
   end subroutine s_parseatom
 
 
 
   subroutine s_getnextatom(func, lenfunc, command, number, order)
     integer, intent(inout) :: lenfunc
     character(lenfunc), intent(inout) :: func
     integer, intent(out) :: command, order
     real(fdp), intent(out) :: number
 
     command = nop_
     order = huge(order)
     number = 0.0d0
 
     if (lenfunc > 0) then
        select case(func(1:1))
           
        case('0':'9', '.')
           call s_readnumber(func, lenfunc, command, number)
           
        case('+', '-', '/', '*', '%', '^', '>', '!', '<', '=')
           call s_readoperator(func, lenfunc, command, order)
           
        case('a':'z','A':'Z')
           call s_readtext(func, lenfunc, command, number)
           
        case('(','[','{')
           command = bracket_
 
        case(',')
           call s_readcomma(func, lenfunc, command, order)
           
        case default
           if (lenfunc > 1) call s_seterror('Unknown input: '//func)
           
           func = ''
           lenfunc = 0
           command = end_
           
        end select
     else
        command = end_
     end if
   end subroutine s_getnextatom
   
 
 
   subroutine s_peeknextatom(func, lenfunc, command, number, order)
     integer, intent(in) :: lenfunc
     character(lenfunc), intent(in) :: func
     integer, intent(out) :: command, order
     real(fdp), intent(out) :: number
     character(len=lenfunc) :: tmpfunc
     integer :: lentmpfunc
 
     command = nop_
     order = huge(order)
     tmpfunc = func
     lentmpfunc = lenfunc
 
     call s_getnextatom(tmpfunc, lentmpfunc, command, number, order)
   end subroutine s_peeknextatom
 
 
 
   recursive subroutine s_parseblock(command, func, lenfunc, funcstack, n, blockorder, components)
     integer, intent(inout) :: lenfunc
     character(lenfunc), intent(inout) :: func
     type(cachefn), intent(inout) :: funcstack
     integer, intent(inout) :: n, components
     integer, intent(in) :: command, blockorder
     integer :: tcommand, nextcommand, nextorder
     real(fdp) :: nextnumber
 
     tcommand = command
     nextcommand = nop_
     nextorder = huge(nextorder)
 
     call s_peeknextatom(func, lenfunc, nextcommand, nextnumber, nextorder)
     call s_checkoperations(func, lenfunc, tcommand, nextcommand)
 
     if ((nextcommand == plus_ .or. nextcommand == minus_) .and. &
          count(command == blockseparator) > 0) call s_funcpush(funcstack, n, number_, 0.0d0)
 
     do while(nextorder > blockorder .and. nextcommand /= end_)
        call s_getnextatom(func, lenfunc, nextcommand, nextnumber, nextorder)
        call s_parseatom(func, lenfunc, funcstack, n, nextcommand, nextnumber, nextorder, components)
        tcommand = nextcommand
        call s_peeknextatom(func, lenfunc, nextcommand, nextnumber, nextorder)
     end do
   end subroutine s_parseblock
 
 
 
   recursive subroutine s_parsefn(func, lenfunc, funcstack, funcstackn, components)
     integer, intent(inout) :: lenfunc
     character(lenfunc), intent(inout) :: func
     type(cachefn), intent(inout) :: funcstack
     integer, intent(inout) :: funcstackn, components
     integer :: order, command
     real(fdp) :: number
 
     call s_getnextatom(func, lenfunc, command, number, order)
     
     if (command == plus_ .or. command == minus_) call s_funcpush(funcstack, funcstackn, number_, 0.0d0)
     
     do while(command /= end_)
        call s_parseatom(func, lenfunc, funcstack, funcstackn, command, number, order, components)
        call s_getnextatom(func, lenfunc, command, number, order)
     end do
   end subroutine s_parsefn
 
#define POP(stack_, stackn_, number_) number_=stack_(stackn_);stackn_=stackn_-1
#define PUSH(stack_, stackn_, number_) stackn_=stackn_+1;stack_(stackn_)=number_
#define FUNCPOP(funcstack_, funcstackn_, command_, number_) funcstackn_=funcstackn_+1;command_=funcstack_%commands(funcstackn_);number_=funcstack_%numbers(funcstackn_)
 
   subroutine s_evaluate(funcstack, funcstackn, varvals, result, errvals)
     type(cachefn), intent(in) :: funcstack
     integer, intent(inout) :: funcstackn
     real(fdp), dimension(:), intent(in) :: varvals
     real(fdp), dimension(:), intent(inout), optional :: errvals
     real(fdp), intent(out) :: result
     real(fdp), dimension(:), allocatable :: tmperrvals
     real(fdp), dimension(:), save, allocatable :: stack
     real(fdp), dimension(maxcommandcomps) :: tmp
     integer :: command, stackn
     real(fdp) :: number
 
     if (.not. allocated(stack)) then
        allocate(stack(0:size(funcstack%numbers)))
        stack = 0.0d0
     elseif (size(stack) < size(funcstack%numbers)+1) then
        deallocate(stack)
        allocate(stack(0:size(funcstack%numbers)))
        stack = 0.0d0
     end if
 
     command = nop_
     number = 0.0d0
     result = 0.0d0
     stackn = 0
 
     if (present(errvals)) then
        allocate(tmperrvals(size(errvals)))
        tmperrvals = errvals
     end if
 
     do while(command /= end_)
        FUNCPOP(funcstack,funcstackn,command,number)
 
        select case(command)
 
        case(number_)
           PUSH(stack,stackn,result)
           result = number
 
        case(variablestart:variableend)
           PUSH(stack,stackn,result)
           result = varvals(command-variablestart)
           if (present(errvals)) then
              tmperrvals(command-variablestart) = errvals(command-variablestart)*number
              result = result+tmperrvals(command-variablestart)
           end if
              
        case(pow_, caret_)
           POP(stack,stackn,tmp(1))
 
           if (tmp(1) > 0.0d0) then
              result = tmp(1) ** result
           elseif (tmp(1) < 0.0d0) then
              if (aint(result) == result) then
                 result = tmp(1) ** nint(result)
              else
                 call s_seterror('x**y, x<0 and y not integer')
              end if
           elseif (result /= 0.0d0) then
              result = 0.0d0
           else             
              call s_seterror('Undefined result 0**0')
           end if
 
        case(neq_)
           POP(stack,stackn,tmp(1))
           result = merge(1.0d0, 0.0d0, tmp(1) /= result)
 
        case(eqeq_, eq_)
           POP(stack,stackn,tmp(1))
           result = merge(1.0d0, 0.0d0, tmp(1) == result)
 
        case(gteq_, eqgt_)
           POP(stack,stackn,tmp(1))
           result = merge(1.0d0, 0.0d0, tmp(1) >= result) 
 
        case(lteq_, eqlt_)
           POP(stack,stackn,tmp(1))
           result = merge(1.0d0, 0.0d0, tmp(1) <= result)
 
        case(gt_)
           POP(stack,stackn,tmp(1))
           result = merge(1.0d0, 0.0d0, tmp(1) > result)
 
        case(lt_)
           POP(stack,stackn,tmp(1))
           result = merge(1.0d0, 0.0d0, tmp(1) < result)
 
        case(mult_)
           POP(stack,stackn,tmp(1))
           result = tmp(1) * result
 
        case(div_)
           POP(stack,stackn,tmp(1))
 
           if (result /= 0.0d0) then
              result = tmp(1) / result
           else
              call s_seterror('x/y, y=0')
           end if
 
        case(idiv_)
           POP(stack,stackn,tmp(1))
 
           if (aint(tmp(1)) /= tmp(1)) then
              call s_seterror('x//y, x not int')
           else if (aint(result) /= result) then
              call s_seterror('x//y, y not int')
           else if (result /= 0.0d0) then
              result = int(tmp(1)) / int(result)
           else
              call s_seterror('x//y, y=0')
           end if
 
        case(perc_)
           POP(stack,stackn,tmp(1))
 
           if (aint(tmp(1)) /= tmp(1)) then
              call s_seterror('x%y, x not int')
           else if (aint(result) /= result) then
              call s_seterror('x%y, y not int')
           else if (result /= 0.0d0) then
              result = modulo(int(tmp(1)),int(result))
           else
              call s_seterror('x%y, y=0')
           end if
 
        case(plus_)
           POP(stack,stackn,tmp(1))
           result = tmp(1) + result
 
        case(minus_)
           POP(stack,stackn,tmp(1))
           result = tmp(1) - result
 
        case(cos_)
           result = cos(result)
 
        case(sin_)
           result = sin(result)
 
        case(tan_)
           result = tan(result)
 
        case(exp_)
           result = exp(result)
 
        case(log_)
           if (result > 0.0d0) then
              result = log(result)
           else
              call s_seterror('log(x), x<=0')
           end if
 
        case(log10_)
           if (result > 0.0d0) then
              result = log10(result)
           else
              call s_seterror('log10(x), x<=0')
           end if
 
        case(sqrt_)
           if (result >= 0.0d0) then
              result = sqrt(result)
           else
              call s_seterror('sqrt(x), x<0')
           end if
 
        case(cbrt_)
           result = f_cbrt(result)
 
        case(acos_)
           if (abs(result) <= 1.0d0) then
              result = acos(result)
           else
              call s_seterror('acos(x), |x|>1')
           end if
 
        case(asin_)
           if (abs(result) <= 1.0d0) then
              result = asin(result)
           else
              call s_seterror('asin(x), |x|>1')
           end if
 
        case(atan_)
           result = atan(result)
 
        case(cosh_)
           result = cosh(result)
 
        case(sinh_)
           result = sinh(result)
 
        case(tanh_)
           result = tanh(result)
 
        case(anint_)
           result = anint(result)
 
        case(aint_)
           result = aint(result)
 
        case(abs_)
           result = abs(result)
 
        case(delta_)
           result = merge(1.0d0, 0.0d0, result == 0.0d0)
 
        case(step_)
           result = merge(0.0d0, 1.0d0, result < 0.0d0)
 
        case(hat_)
           result = merge(1.0d0, 0.0d0, result <= 0.5d0 .and. result >= -0.5d0)
 
        case(max_)
           POP(stack,stackn,tmp(1))
           result = max(result, tmp(1))
 
        case(min_)
           POP(stack,stackn,tmp(1))
           result = min(result, tmp(1))
 
        case(besj0_)
           result = f_besj0(result)
 
        case(besj1_)
           result = f_besj1(result)
 
        case(besjn_)
           POP(stack,stackn,tmp(1))
           if (aint(tmp(1)) == tmp(1)) then
              result = f_besjn(tmp(1), result)
           else
              call s_seterror('besjn(n, x), n not integer')
           end if
 
        case(besy0_)
           result = f_besy0(result)
 
        case(besy1_)
           result = f_besy1(result)
 
        case(besyn_)
           POP(stack,stackn,tmp(1))
           if (aint(tmp(1)) == tmp(1) .and. result > 0.0d0) then
              result = f_besyn(tmp(1), result)
           else
              call s_seterror('besyn(n, x), n not integer or x/>0')
           end if
           
        case(logn_)
           POP(stack,stackn,tmp(1))
 
           if (result > 0.0d0 .and. tmp(1) > 0.0d0) then
              result = log(result)/log(tmp(1))
           else
              call s_seterror('logn(x, y), x/>0 or y/>0')
           end if
 
        case(erf_)
           result = f_erf(result)
 
        case(erfc_)
           result = 1.0d0 - f_erf(result)
 
        case(lgamma_)
           if (result > 0.0d0) then
              result = f_lgamma(result)
           else
              call s_seterror('lgamma(x), x<=0')
           end if
 
        case(gamma_)
           result = f_gamma(result)
 
        case(csch_)
           result = sinh(result)
 
           if (result == 0.0d0) then
              call s_seterror('1/sinh(x), sinh(x) = 0')
           else
              result = 1.0d0/result
           end if
 
        case(sech_)
           result = 1.0d0/cosh(result)
 
        case(coth_)
           result = tanh(result)
 
           if (result == 0.0d0) then
              call s_seterror('1/tanh(x), tanh(x) = 0')
           else
              result = 1.0d0/result
           end if
 
        case(if_)
           POP(stack,stackn,tmp(1))
           POP(stack,stackn,tmp(2))
 
           result = merge(result, tmp(1), tmp(2) == 0)
 
        case(gauss_)
           result = exp(-result**2)
 
        case(sinc_)
           result = merge(1.0d0, sin(result)/result, result == 0.0d0)
 
        case(besi0_)
           result = f_besi0(result)
 
        case(besi1_)
           result = f_besi1(result)
           
        case(besin_)
           POP(stack,stackn,tmp(1))
           if (aint(tmp(1)) == tmp(1)) then
              result = f_besin(tmp(1), result)
           else
              call s_seterror('besin(n, x), n not integer')
           end if
 
        case(besk0_)
           result = f_besk0(result)
           
        case(besk1_)
           result = f_besk1(result)
           
        case(beskn_)
           POP(stack,stackn,tmp(1))
           if (aint(tmp(1)) == tmp(1)) then
              result = f_beskn(tmp(1), result)
           else
              call s_seterror('besjk(n, x), n not integer')
           end if
 
        case(ierfc_)
           if (abs(result-1.0d0) < 1.0d0) then
              result = f_ierfc(result)
           else
              call s_seterror('ierfc(x), 0 < x < 2')
           end if
 
        case(fresc_)
           result = f_fresc(result)
 
        case(fress_)
           result = f_fress(result)
 
        case(expi_)
           result = f_expi(result)
 
        case(sini_)
           result = f_sini(result)
 
        case(cosi_)
           result = f_cosi(result)
 
        case(logi_)
           if (result > 0.0d0) then
              result = f_logi(result)
           else
              call s_seterror('logi(x), x > 0')
           end if
 
        case(ierf_)
           if (abs(result) < 1.0d0) then
              result = f_ierf(result)
           else
              call s_seterror('ierf(x), -1 < x < 1')
           end if
 
        case(elle_)
           if (abs(result) < 1.0d0) then
              result = f_elle(result)
           else
              call s_seterror('elle(x), -1 < x < 1')
           end if
 
        case(ellk_)
           if (abs(result) < 1.0d0) then
              result = f_ellk(result)
           else
              call s_seterror('ellk(x), -1 < x < 1')
           end if
 
        case(ielle_)
           POP(stack,stackn,tmp(1))
           if (abs(tmp(1)) < 1.0d0 .and. abs(result) < pi2) then
              result = f_ielle(tmp(1), result)
           else
              call s_seterror('ielle(x, phi), -1 < x < 1, -pi/2 < phi < pi/2')
           end if
 
        case(iellf_)
           POP(stack,stackn,tmp(1))
           if (abs(tmp(1)) < 1.0d0 .and. abs(result) < pi2) then
              result = f_iellf(tmp(1), result)
           else
              call s_seterror('iellf(x, phi), -1 < x < 1, -pi/2 < phi < pi/2')
           end if
 
        case(modulo_)
           POP(stack,stackn,tmp(1))
           if(result /= 0.0d0) then
              result = modulo(tmp(1), result)
           else
              call s_seterror('modulo(x, y), y=0')
           end if
 
        case(floor_)
           result = floor(result)
 
        case(ceiling_)
           result = ceiling(result)
 
        case(mod_)
           POP(stack,stackn,tmp(1))
           if(result /= 0.0d0) then
              result = mod(tmp(1), result)
           else
              call s_seterror('mod(x,y), y=0')
           end if
 
        end select
     end do
 
     if (stackn > 1) call s_seterror('Evaluation stack not empty')
 
     if (present(errvals)) then
        errvals = tmperrvals
        deallocate(tmperrvals)
     end if
   end subroutine s_evaluate
 
 
 
   subroutine s_variableerr(funcstack, number, varvals, errval, i)
     type(cachefn), intent(in) :: funcstack
     real(fdp), intent(inout) :: number
     real(fdp), dimension(:), intent(in) :: varvals
     real(fdp), intent(in) :: errval
     integer, intent(in) :: i
     real(fdp), dimension(size(varvals)) :: errvals
     real(fdp) :: numplus, numminus, rangeplus, rangeminus
     integer :: funcstackn
 
     number = 0.0d0
 
     if (error == 'OK' .and. size(varvals) > 0) then
        !
        ! Compute actual error
        !
 
        errvals = 0.0d0
        errvals(i) = errval
        funcstackn = 0
 
        call s_evaluate(funcstack, funcstackn, varvals, numplus, errvals)
 
        rangeplus = errvals(i)
        errvals(i) = -errval
        funcstackn = 0
 
        call s_evaluate(funcstack, funcstackn, varvals, numminus, errvals)
 
        rangeminus = errvals(i)
 
        number = merge(0.5d0*(numplus*rangeminus**2-numminus*rangeplus**2-&
             number*(rangeminus+rangeplus)**2)/rangeplus/rangeminus, &
             0.0d0, (rangeminus /= 0) .and. (rangeplus /= 0))
     end if
   end subroutine s_variableerr
   
 
 
   subroutine s_createfn(func, vars, funcnumber, t_error)
     character(len=*), intent(in) :: func, vars
     integer, intent(out) :: funcnumber
     character(len=*), intent(out), optional :: t_error
     type(cachefn), pointer :: funcstack
     type(cachefn), dimension(:), pointer :: tmpcachedfuncs
     integer :: funcstackn, i, j, n, lentmpfunc, components
     character(len=len(func)) :: tmpfunc
     
     funcnumber = 0
     error = 'OK'
 
     call s_firstrun()
 
     if (trim(vars) /= "") call s_allocatevariables(vars)
     
     if (error == 'OK') then
        n = size(cachedfuncs)
 
        do i=1, n
           if (.not. associated(cachedfuncs(i)%numbers)) exit
        end do
 
        if (i > n) then
           allocate(tmpcachedfuncs(i+10))
 
           do j = 1, n
              tmpcachedfuncs(j)%commands => cachedfuncs(j)%commands
              tmpcachedfuncs(j)%numbers => cachedfuncs(j)%numbers
           end do
           
           do j = n+1, size(tmpcachedfuncs)
              nullify(tmpcachedfuncs(j)%commands, tmpcachedfuncs(j)%numbers)
           end do
 
           deallocate(cachedfuncs)
           cachedfuncs => tmpcachedfuncs
        end if
 
        funcnumber = i
        funcstack => cachedfuncs(i)
        funcstackn = 0
        tmpfunc = f_clean(func)
        lentmpfunc = len_trim(tmpfunc)
        
        allocate(funcstack%commands(10), funcstack%numbers(10))
        
        funcstack%commands = end_
        funcstack%numbers = 0.0d0
        funcstackn = 0
        components = 0
 
        call s_parsefn(tmpfunc, lentmpfunc, funcstack, funcstackn, components)
 
 !       if (components > 1) call s_seterror('Multiple component function')
     end if
 
     if (trim(vars) /= "") call s_deallocatevariables()
     
     if (present(t_error)) t_error = error
   end subroutine s_createfn
 
 
 
   subroutine s_destroyfn(funcnumber)
     integer, intent(in) :: funcnumber
 
     call s_firstrun()
 
     if (funcnumber < 1) then
        call s_seterror('Function does not exist')
     else if (funcnumber > size(cachedfuncs)) then
        call s_seterror('Function does not exist')
     else if (associated(cachedfuncs(funcnumber)%commands)) then
        deallocate(cachedfuncs(funcnumber)%numbers, cachedfuncs(funcnumber)%commands)
     else
        call s_seterror('Function does not exist')
     end if
   end subroutine s_destroyfn
   
   
   
   subroutine s_evaluatefn(funcnumber, varvals, number, t_error)
     integer, intent(in) :: funcnumber
     real(fdp), dimension(:), intent(in) :: varvals
     real(fdp), intent(out) :: number
     character(len=*), intent(out), optional :: t_error
     type(cachefn) :: funcstack
     integer :: funcstackn
 
     error = 'OK'
     
     call s_firstrun()
 
     number = 0.0d0
     if (present(t_error)) t_error = ''
     
     if (funcnumber > size(cachedfuncs) .or. funcnumber < 1) then
        call s_seterror('Function does not exist')
     else if (associated(cachedfuncs(funcnumber)%commands)) then
        funcstack = cachedfuncs(funcnumber)
        funcstackn = 0
 
        call s_evaluate(funcstack, funcstackn, varvals, number)
     else
        call s_seterror('Function does not exist')
     end if
 
     if (present(t_error)) t_error = error
   end subroutine s_evaluatefn
   
   
   
   subroutine s_evaluateerr(funcnumber, varvals, number, t_error)
     integer, intent(in) :: funcnumber
     real(fdp), dimension(:), intent(in) :: varvals
     real(fdp), intent(out) :: number
     character(len=*), intent(out), optional :: t_error
     type(cachefn) :: funcstack
     real(fdp), dimension(size(varvals)) :: errvals
     real(fdp) :: num, halfnumber, tmp
     integer :: funcstackn, i
 
     error = 'OK'
 
     call s_firstrun()
 
     number = 0.0d0
     if (present(t_error)) t_error = ''
     halfnumber = 0.0d0
 
     if (funcnumber > size(cachedfuncs) .or. funcnumber < 1) then
        call s_seterror('Function does not exist')
     else if (associated(cachedfuncs(funcnumber)%commands)) then
        funcstack = cachedfuncs(funcnumber)
        errvals = 0.0d0
        funcstackn = 0
        
        call s_evaluate(funcstack, funcstackn, varvals, num, errvals)
 
        do i = 1, size(varvals)
           tmp = num
 
           call s_variableerr(funcstack, tmp, varvals, 1.0d0, i)
 
           number = number + abs(tmp)
 
           !
           ! Compute the error with half the range
           !
 
           tmp = num
 
           call s_variableerr(funcstack, tmp, varvals, 0.5d0, i)
 
           halfnumber = halfnumber + abs(tmp)
        end do
 
        !
        ! Return correct precision error
        !
 
        halfnumber = 2.0d0*halfnumber
 
        call s_errprecision(number)
        call s_errprecision(halfnumber)
 
        !
        ! Verify error value is consistent
        !
 
        if (halfnumber /= number) call s_seterror('Error value not stable')
     else
        call s_seterror('Function does not exist')
     end if
     
     if (present(t_error)) t_error = error
   end subroutine s_evaluateerr
   
   
   
 end module interpreter
 
 
 
 ! ========================================================
 !
 !  Include some stubs to allow non-module library linking
 !
 ! ========================================================
 
 
 
   subroutine createfn(func, vars, funcnumber, t_error)
     use interpreter
 
     character(*) :: func, vars
     integer :: funcnumber
     character(*), optional :: t_error
 
     call s_createfn(func, vars, funcnumber, t_error)
   end subroutine createfn
 
 
 
   subroutine destroyfn(funcnumber)
     use interpreter
 
     integer :: funcnumber
 
     call s_destroyfn(funcnumber)
   end subroutine destroyfn
 
 
 
   subroutine evaluatefn(funcnumber, varvals, number, t_error)
     use interpreter
 
     integer :: funcnumber
     real(8), dimension(:) :: varvals
     real(8) :: number
     character(*), optional :: t_error
 
     call s_evaluatefn(funcnumber, varvals, number, t_error)
   end subroutine evaluatefn
 
 
 
   subroutine evaluateerr(funcnumber, varvals, number, t_error)
     use interpreter
 
     integer :: funcnumber
     real(8), dimension(:) :: varvals
     real(8) :: number
     character(*),optional :: t_error
 
     call s_evaluateerr(funcnumber, varvals, number, t_error)
   end subroutine evaluateerr