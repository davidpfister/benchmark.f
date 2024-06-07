!
! Copyright (c) 2000-2008, Roland Schmehl. All rights reserved.
!
! This software is distributable under the BSD license. See the terms of the
! BSD license in the documentation provided with this software.
!
module FortranParser
    !------- -------- --------- --------- --------- --------- --------- --------- -------
    ! Fortran 2008 function parser
    !------- -------- --------- --------- --------- --------- --------- --------- -------
    !
    ! This is an OOP Fortran 2008 version of the original fparser by Roland Schmehl. This simple class
    ! wrapping of the original fparser has been developed by Jacopo Chevallard, and it is available on
    ! the GitHub repository https://github.com/jacopo-chevallard/FortranParser.
    !
    ! For comments and bug reports, please open an issue on
    ! https://github.com/jacopo-chevallard/FortranParser/issues
    !
    ! This function parser module is intended for applications where a set of mathematical
    ! fortran-style expressions is specified at runtime and is then evaluated for a large
    ! number of variable values. This is done by compiling the set of function strings
    ! into byte code, which is interpreted efficiently for the various variable values.
    !
    ! The source code of the original fparser is available from http://fparser.sourceforge.net
    !
    ! Please send comments, corrections or questions realtive to the original fparser to its author:
    ! Roland Schmehl <roland.schmehl@alumni.uni-karlsruhe.de>
    !
    !------- -------- --------- --------- --------- --------- --------- --------- -------
    ! The function parser concept is based on a C++ class library written by  Juha
    ! Nieminen <warp@iki.fi> available from http://warp.povusers.org/FunctionParser/
    !------- -------- --------- --------- --------- --------- --------- --------- -------
    use FortranParser_parameters, only: rn, is ! Import KIND parameters

    implicit none

    public                     :: EquationParser

    !------- -------- --------- --------- --------- --------- --------- --------- -------
    private

    integer(is), parameter :: cImmed = 1, &
                              cNeg = 2, &
                              cAdd = 3, &
                              cSub = 4, &
                              cMul = 5, &
                              cDiv = 6, &
                              cPow = 7, &
                              cAbs = 8, &
                              cExp = 9, &
                              cLog10 = 10, &
                              cLog = 11, &
                              cSqrt = 12, &
                              cSinh = 13, &
                              cCosh = 14, &
                              cTanh = 15, &
                              cSin = 16, &
                              cCos = 17, &
                              cTan = 18, &
                              cAsin = 19, &
                              cAcos = 20, &
                              cAtan = 21, &
                              VarBegin = 22

    character(LEN=1), dimension(cAdd:cPow), parameter :: Ops = (/'+', &
                                                                 '-', &
                                                                 '*', &
                                                                 '/', &
                                                                 '^'/)

    character(LEN=5), dimension(cAbs:cAtan), parameter :: Funcs = (/'abs  ', &
                                                                    'exp  ', &
                                                                    'log10', &
                                                                    'log  ', &
                                                                    'sqrt ', &
                                                                    'sinh ', &
                                                                    'cosh ', &
                                                                    'tanh ', &
                                                                    'sin  ', &
                                                                    'cos  ', &
                                                                    'tan  ', &
                                                                    'asin ', &
                                                                    'acos ', &
                                                                    'atan '/)

    integer, parameter  :: MAX_FUN_LENGTH = 1024

    type EquationParser

        integer(is), allocatable :: ByteCode(:)
        integer                  :: ByteCodeSize = 0
        real(rn), allocatable    :: Immed(:)
        integer                  :: ImmedSize = 0
        real(rn), allocatable    :: Stack(:)
        integer                  :: StackSize = 0
        integer                  :: StackPtr = 0

        character(len=MAX_FUN_LENGTH) :: funcString = ''
        character(len=MAX_FUN_LENGTH) :: funcStringOrig = ''
        character(len=MAX_FUN_LENGTH), allocatable :: variableNames(:)
    contains

        private

        procedure, public :: evaluate
        procedure:: parse
        procedure :: Compile
        procedure :: AddCompiledByte
        procedure :: CompileSubstr
        procedure :: MathItemIndex
        procedure :: CheckSyntax

    end type EquationParser

    ! Class constructor
    interface EquationParser
        procedure constructor
    end interface EquationParser

contains

!*****************************************************************************************
    type(EquationParser) function constructor(FuncStr, Var)

        character(LEN=*), intent(in) :: FuncStr ! Function string
        character(LEN=*), dimension(:), intent(in) :: Var ! Array with variable names

        constructor%funcString = FuncStr
        constructor%funcStringOrig = FuncStr

        allocate (constructor%variableNames(size(Var)))

        constructor%variableNames = Var

        call constructor%parse()

    end function constructor

!*****************************************************************************************
    subroutine parse(this)
        ! Parse ith function string FuncStr and compile it into bytecode
        class(EquationParser) :: this

        call Replace('**', '^ ', this%funcString) ! Exponent into 1-Char. format

        call RemoveSpaces(this%funcString) ! Condense function string

        call this%CheckSyntax()

        call this%Compile() ! Compile into bytecode

    end subroutine parse

!*****************************************************************************************
    function evaluate(this, Val) result(res)
        ! Evaluate bytecode of ith function for the values passed in array Val(:)
        class(EquationParser) :: this
        real(rn), dimension(:), intent(in) :: Val ! Variable values

        real(rn)                           :: res ! Result
        integer                            :: IP, & ! Instruction pointer
                                              DP, & ! Data pointer
                                              SP ! Stack pointer
        real(rn), parameter :: zero = 0._rn
        integer       :: EvalErrType

        DP = 1
        SP = 0
        EvalErrType = 0

        do IP = 1, this%ByteCodeSize

            select case (this%ByteCode(IP))

            case (cImmed); SP = SP + 1; this%Stack(SP) = this%Immed(DP); DP = DP + 1

            case (cNeg); this%Stack(SP) = -this%Stack(SP)

            case (cAdd); this%Stack(SP - 1) = this%Stack(SP - 1) + this%Stack(SP); SP = SP - 1

            case (cSub); this%Stack(SP - 1) = this%Stack(SP - 1) - this%Stack(SP); SP = SP - 1

            case (cMul); this%Stack(SP - 1) = this%Stack(SP - 1) * this%Stack(SP); SP = SP - 1

            case (cDiv)

                if (this%Stack(SP) == 0._rn) then
                    EvalErrType = 1
                    res = zero
                    exit
                end if
                this%Stack(SP - 1) = this%Stack(SP - 1) / this%Stack(SP); SP = SP - 1

            case (cPow); this%Stack(SP - 1) = this%Stack(SP - 1)**this%Stack(SP); SP = SP - 1

            case (cAbs); this%Stack(SP) = abs(this%Stack(SP))

            case (cExp); this%Stack(SP) = exp(this%Stack(SP))

            case (cLog10)

                if (this%Stack(SP) <= 0._rn) then
                    EvalErrType = 3
                    res = zero
                    exit
                end if
                this%Stack(SP) = log10(this%Stack(SP))

            case (cLog)

                if (this%Stack(SP) <= 0._rn) then
                    EvalErrType = 3
                    res = zero
                    exit
                end if
                this%Stack(SP) = log(this%Stack(SP))

            case (cSqrt)

                if (this%Stack(SP) < 0._rn) then
                    EvalErrType = 3
                    res = zero
                    exit
                end if
                this%Stack(SP) = sqrt(this%Stack(SP))

            case (cSinh); this%Stack(SP) = sinh(this%Stack(SP))

            case (cCosh); this%Stack(SP) = cosh(this%Stack(SP))

            case (cTanh); this%Stack(SP) = tanh(this%Stack(SP))

            case (cSin); this%Stack(SP) = sin(this%Stack(SP))

            case (cCos); this%Stack(SP) = cos(this%Stack(SP))

            case (cTan); this%Stack(SP) = tan(this%Stack(SP))

            case (cAsin)

                if ((this%Stack(SP) < -1._rn) .or. (this%Stack(SP) > 1._rn)) then
                    EvalErrType = 4
                    res = zero
                    exit
                end if
                this%Stack(SP) = asin(this%Stack(SP))

            case (cAcos); 
                if ((this%Stack(SP) < -1._rn) .or. (this%Stack(SP) > 1._rn)) then
                    EvalErrType = 4
                    res = zero
                    exit
                end if
                this%Stack(SP) = acos(this%Stack(SP))

            case (cAtan); this%Stack(SP) = atan(this%Stack(SP))

            case DEFAULT; SP = SP + 1; this%Stack(SP) = Val(this%ByteCode(IP) - VarBegin + 1)

            end select

        end do

        if (EvalErrType > 0) then
            write (*, *) '*** Error: ', EvalErrMsg(EvalErrType)
        else
            res = this%Stack(1)
        end if

    end function evaluate

!*****************************************************************************************
    subroutine CheckSyntax(this)
        ! Check syntax of function string,  returns 0 if syntax is ok
        class(EquationParser) :: this
        integer(is)                                 :: n
        character(LEN=1)                           :: c
        real(rn)                                    :: r
        logical                                     :: err
        integer                                     :: ParCnt, & ! Parenthesis counter
                                                       j, ib, in, lFunc

        j = 1
        ParCnt = 0
        lFunc = len_trim(this%funcString)
        step: do
            if (j > lFunc) call ParseErrMsg(j, this%funcStringOrig)
            c = this%funcString(j:j)
            !-- -------- --------- --------- --------- --------- --------- --------- -------
            ! Check for valid operand (must appear)
            !-- -------- --------- --------- --------- --------- --------- --------- -------
            if (c == '-' .or. c == '+') then ! Check for leading - or +
                j = j + 1
                if (j > lFunc) call ParseErrMsg(j, this%funcStringOrig, 'Missing operand')
                c = this%funcString(j:j)
                if (any(c == Ops)) call ParseErrMsg(j, this%funcStringOrig, 'Multiple operators')
            end if
            n = MathFunctionIndex(this%funcString(j:))
            if (n > 0) then ! Check for math function
                j = j + len_trim(Funcs(n))
                if (j > lFunc) call ParseErrMsg(j, this%funcStringOrig, 'Missing function argument')
                c = this%funcString(j:j)
                if (c /= '(') call ParseErrMsg(j, this%funcStringOrig, 'Missing opening parenthesis')
            end if
            if (c == '(') then ! Check for opening parenthesis
                ParCnt = ParCnt + 1
                j = j + 1
                cycle step
            end if
            if (scan(c, '0123456789.') > 0) then ! Check for number
                r = RealNum(this%funcString(j:), ib, in, err)
                if (err) call ParseErrMsg(j, this%funcStringOrig, 'Invalid number format:  '//this%funcString(j + ib - 1:j + in - 2))
                j = j + in - 1
                if (j > lFunc) exit
                c = this%funcString(j:j)
            else ! Check for variable
                n = VariableIndex(this%funcString(j:), this%variableNames, ib, in)
                if (n == 0) call ParseErrMsg(j, this%funcStringOrig, 'Invalid element: '//this%funcString(j + ib - 1:j + in - 2))
                j = j + in - 1
                if (j > lFunc) exit
                c = this%funcString(j:j)
            end if
            do while (c == ')') ! Check for closing parenthesis
                ParCnt = ParCnt - 1
                if (ParCnt < 0) call ParseErrMsg(j, this%funcStringOrig, 'Mismatched parenthesis')
                if (this%funcString(j - 1:j - 1) == '(') call ParseErrMsg(j - 1, this%funcStringOrig, 'Empty parentheses')
                j = j + 1
                if (j > lFunc) exit
                c = this%funcString(j:j)
            end do
            !-- -------- --------- --------- --------- --------- --------- --------- -------
            ! Now, we have a legal operand: A legal operator or end of string must follow
            !-- -------- --------- --------- --------- --------- --------- --------- -------
            if (j > lFunc) exit
            if (any(c == Ops)) then ! Check for multiple operators
                if (j + 1 > lFunc) call ParseErrMsg(j, this%funcStringOrig)
                if (any(this%funcString(j + 1:j + 1) == Ops)) call ParseErrMsg(j + 1, this%funcStringOrig, 'Multiple operators')
            else ! Check for next operand
                call ParseErrMsg(j, this%funcStringOrig, 'Missing operator')
            end if
            !-- -------- --------- --------- --------- --------- --------- --------- -------
            ! Now, we have an operand and an operator: the next loop will check for another
            ! operand (must appear)
            !-- -------- --------- --------- --------- --------- --------- --------- -------
            j = j + 1
        end do step
        if (ParCnt > 0) call ParseErrMsg(j, this%funcStringOrig, 'Missing )')
    end subroutine CheckSyntax

!*****************************************************************************************
    function EvalErrMsg(EvalErrType) result(msg)
        ! Return error message
        integer, intent(in) :: EvalErrType
        character(LEN=*), dimension(4), parameter :: m = (/'Division by zero                ', &
                                                           'Argument of SQRT negative       ', &
                                                           'Argument of LOG negative        ', &
                                                           'Argument of ASIN or ACOS illegal'/)
        character(LEN=len(m))                     :: msg
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        if (EvalErrType < 1 .or. EvalErrType > size(m)) then
            msg = ''
        else
            msg = m(EvalErrType)
        end if

    end function EvalErrMsg

!*****************************************************************************************
    subroutine ParseErrMsg(j, FuncStr, Msg)
        ! Print error message and terminate program
        integer, intent(in) :: j
        character(LEN=*), intent(in) :: FuncStr ! Original function string
        character(LEN=*), optional, intent(in) :: Msg

        if (present(Msg)) then
            write (*, *) '*** Error in syntax of function string: '//Msg
        else
            write (*, *) '*** Error in syntax of function string:'
        end if

        write (*, *)
        write (*, '(A)') ' '//FuncStr

        write (*, '(A)') '?'
        stop
    end subroutine ParseErrMsg

!*****************************************************************************************
    function OperatorIndex(c) result(n)
        ! Return operator index
        character(LEN=1), intent(in) :: c
        integer(is)                   :: n, j

        n = 0

        do j = cAdd, cPow
            if (c == Ops(j)) then
                n = j
                exit
            end if
        end do

    end function OperatorIndex

!*****************************************************************************************
    function MathFunctionIndex(str) result(n)
        ! Return index of math function beginnig at 1st position of string str
        character(LEN=*), intent(in) :: str

        integer(is)                   :: n, j
        integer                       :: k
        character(LEN=len(Funcs))    :: fun

        n = 0

        do j = cAbs, cAtan ! Check all math functions
            k = min(len_trim(Funcs(j)), len(str))
            call LowCase(str(1:k), fun)
            if (fun == Funcs(j)) then ! Compare lower case letters
                n = j ! Found a matching function
                exit
            end if
        end do

    end function MathFunctionIndex

!*****************************************************************************************
    function VariableIndex(str, Var, ibegin, inext) result(n)
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        ! Return index of variable at begin of string str (returns 0 if no variable found)
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        implicit none
        character(LEN=*), intent(in) :: str ! String
        character(LEN=*), dimension(:), intent(in) :: Var ! Array with variable names
        integer(is)                                 :: n ! Index of variable
        integer, optional, intent(out) :: ibegin, & ! Start position of variable name
                                          inext ! Position of character after name
        integer                                     :: j, ib, in, lstr
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        n = 0
        lstr = len_trim(str)
        if (lstr > 0) then
            do ib = 1, lstr ! Search for first character in str
                if (str(ib:ib) /= ' ') exit ! When lstr>0 at least 1 char in str
            end do
            do in = ib, lstr ! Search for name terminators
                if (scan(str(in:in), '+-*/^) ') > 0) exit
            end do
            do j = 1, size(Var)
                if (str(ib:in - 1) == Var(j)) then
                    n = int(j, is) ! Variable name found
                    exit
                end if
            end do
        end if
        if (present(ibegin)) ibegin = ib
        if (present(inext)) inext = in
    end function VariableIndex

!*****************************************************************************************
    subroutine RemoveSpaces(str)
        ! Remove Spaces from string, remember positions of characters in old string
        character(LEN=*), intent(inout) :: str

        integer                          :: k, lstr

        lstr = len_trim(str)

        k = 1

        do while (str(k:lstr) /= ' ')
            if (str(k:k) == ' ') then
                str(k:lstr) = str(k + 1:lstr)//' ' ! Move 1 character to left
                k = k - 1
            end if
            k = k + 1
        end do

    end subroutine RemoveSpaces

!*****************************************************************************************
    subroutine Replace(ca, cb, str)
        ! Replace ALL appearances of character set ca in string str by character set cb
        character(LEN=*), intent(in) :: ca
        character(LEN=len(ca)), intent(in) :: cb ! LEN(ca) must be LEN(cb)
        character(LEN=*), intent(inout) :: str

        integer                             :: j, lca

        lca = len(ca)

        do j = 1, len_trim(str) - lca + 1
            if (str(j:j + lca - 1) == ca) str(j:j + lca - 1) = cb
        end do

    end subroutine Replace

!*****************************************************************************************
    subroutine Compile(this)
        ! Compile i-th function string F into bytecode
        class(EquationParser) :: this
        integer                                     :: istat

        if (allocated(this%ByteCode)) deallocate (this%ByteCode, &
                                                  this%Immed, &
                                                  this%Stack)
        this%ByteCodeSize = 0
        this%ImmedSize = 0
        this%StackSize = 0
        this%StackPtr = 0

        call this%CompileSubstr(1, len_trim(this%funcString)) ! Compile string to determine size

        allocate (this%ByteCode(this%ByteCodeSize), &
                  this%Immed(this%ImmedSize), &
                  this%Stack(this%StackSize), &
                  STAT=istat)
        if (istat /= 0) then
            write (*, *) '*** Parser error: Memmory allocation for byte code failed'
            stop
        else
            this%ByteCodeSize = 0
            this%ImmedSize = 0
            this%StackSize = 0
            this%StackPtr = 0
            call this%CompileSubstr(1, len_trim(this%funcString)) ! Compile string into bytecode
        end if

    end subroutine Compile

!*****************************************************************************************
    subroutine AddCompiledByte(this, b)
        ! Add compiled byte to bytecode
        class(EquationParser) :: this
        integer(is), intent(in) :: b ! Value of byte to be added

        this%ByteCodeSize = this%ByteCodeSize + 1

        if (allocated(this%ByteCode)) then
            this%ByteCode(this%ByteCodeSize) = b
        end if

    end subroutine AddCompiledByte

!*****************************************************************************************
    function MathItemIndex(this, b, e) result(n)
        ! Return math item index, if item is real number, enter it into Comp-structure
        class(EquationParser) :: this

        integer, intent(in) :: b, e ! First and last pos. of substring
        integer(is)                                 :: n ! Byte value of math item

        n = 0

        if (scan(this%funcString(b:b), '0123456789.') > 0) then ! Check for begin of a number
            this%ImmedSize = this%ImmedSize + 1
            if (allocated(this%Immed)) this%Immed(this%ImmedSize) = RealNum(this%funcString(b:e))
            n = cImmed
        else ! Check for a variable
            n = VariableIndex(this%funcString(b:e), this%variableNames)
            if (n > 0) n = VarBegin + n - 1_is
        end if

    end function MathItemIndex

!*****************************************************************************************
    function CompletelyEnclosed(F, b, e) result(res)
        ! Check if function substring F(b:e) is completely enclosed by a pair of parenthesis
        character(LEN=*), intent(in) :: F ! Function substring
        integer, intent(in) :: b, e ! First and last pos. of substring

        logical                       :: res
        integer                       :: j, k

        res = .false.

        if (F(b:b) == '(' .and. F(e:e) == ')') then
            k = 0
            do j = b + 1, e - 1
                if (F(j:j) == '(') then
                    k = k + 1
                elseif (F(j:j) == ')') then
                    k = k - 1
                end if
                if (k < 0) exit
            end do
            if (k == 0) res = .true. ! All opened parenthesis closed
        end if

    end function CompletelyEnclosed

!*****************************************************************************************
    recursive subroutine CompileSubstr(this, b, e)
        ! Compile i-th function string funcString into bytecode
        class(EquationParser) :: this
        integer, intent(in) :: b, e ! Begin and end position substring

        integer(is)                                 :: n
        integer                                     :: b2, j, k, io
        character(LEN=*), parameter :: calpha = 'abcdefghijklmnopqrstuvwxyz'// &
                                       'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
        ! Check for special cases of substring

        if (this%funcString(b:b) == '+') then ! Case 1: funcString(b:e) = '+...'
!      WRITE(*,*)'1. funcString(b:e) = "+..."'
            call this%CompileSubstr(b + 1, e)
            return
        elseif (CompletelyEnclosed(this%funcString, b, e)) then ! Case 2: funcString(b:e) = '(...)'
!      WRITE(*,*)'2. funcString(b:e) = "(...)"'
            call this%CompileSubstr(b + 1, e - 1)
            return
        elseif (scan(this%funcString(b:b), calpha) > 0) then
            n = MathFunctionIndex(this%funcString(b:e))
            if (n > 0) then
                b2 = b + index(this%funcString(b:e), '(') - 1
                if (CompletelyEnclosed(this%funcString, b2, e)) then ! Case 3: funcString(b:e) = 'fcn(...)'
!            WRITE(*,*)'3. funcString(b:e) = "fcn(...)"'
                    call this%CompileSubstr(b2 + 1, e - 1)
                    call this%AddCompiledByte(n)
                    return
                end if
            end if

        elseif (this%funcString(b:b) == '-') then
            if (CompletelyEnclosed(this%funcString, b + 1, e)) then ! Case 4: this%funcString(b:e) = '-(...)'
!         WRITE(*,*)'4. this%funcString(b:e) = "-(...)"'
                call this%CompileSubstr(b + 2, e - 1)
                call this%AddCompiledByte(cNeg)
                return
            elseif (scan(this%funcString(b + 1:b + 1), calpha) > 0) then
                n = MathFunctionIndex(this%funcString(b + 1:e))
                if (n > 0) then
                    b2 = b + index(this%funcString(b + 1:e), '(')
                    if (CompletelyEnclosed(this%funcString, b2, e)) then ! Case 5: this%funcString(b:e) = '-fcn(...)'
!               WRITE(*,*)'5. this%funcString(b:e) = "-fcn(...)"'
                        call this%CompileSubstr(b2 + 1, e - 1); 
                        call this%AddCompiledByte(n)
                        call this%AddCompiledByte(cNeg)
                        return
                    end if
                end if
            end if
        end if

        ! Check for operator in substring: check only base level (k=0), exclude expr. in ()

        do io = cAdd, cPow ! Increasing priority +-*/^
            k = 0
            do j = e, b, -1
                if (this%funcString(j:j) == ')') then
                    k = k + 1
                elseif (this%funcString(j:j) == '(') then
                    k = k - 1
                end if
                if (k == 0 .and. this%funcString(j:j) == Ops(io) .and. IsBinaryOp(j, this%funcString)) then
                    if (any(this%funcString(j:j) == Ops(cMul:cPow)) .and. this%funcString(b:b) == '-') then ! Case 6: this%funcString(b:e) = '-...Op...' with Op > -
!               WRITE(*,*)'6. this%funcString(b:e) = "-...Op..." with Op > -'
                        call this%CompileSubstr(b + 1, e)
                        call this%AddCompiledByte(cNeg)
                        return
                    else ! Case 7: this%funcString(b:e) = '...BinOp...'
!               WRITE(*,*)'7. Binary operator',this%funcString(j:j)
                        call this%CompileSubstr(b, j - 1)
                        call this%CompileSubstr(j + 1, e)
                        call this%AddCompiledByte(OperatorIndex(Ops(io)))
                        this%StackPtr = this%StackPtr - 1
                        return
                    end if
                end if
            end do
        end do

        ! Check for remaining items, i.e. variables or explicit numbers

        b2 = b

        if (this%funcString(b:b) == '-') b2 = b2 + 1

        n = this%MathItemIndex(b2, e)

!   WRITE(*,*)'8. AddCompiledByte ',n
        call this%AddCompiledByte(n)

        this%StackPtr = this%StackPtr + 1
        if (this%StackPtr > this%StackSize) this%StackSize = this%StackSize + 1

        if (b2 > b) call this%AddCompiledByte(cNeg)

    end subroutine CompileSubstr

!*****************************************************************************************
    function IsBinaryOp(j, F) result(res)
        ! Check if operator F(j:j) in string F is binary operator
        ! Special cases already covered elsewhere:              (that is corrected in v1.1)
        ! - operator character F(j:j) is first character of string (j=1)
        integer, intent(in) :: j ! Position of Operator
        character(LEN=*), intent(in) :: F ! String

        logical                       :: res ! Result
        integer                       :: k
        logical                       :: Dflag, Pflag

        res = .true.

        if (F(j:j) == '+' .or. F(j:j) == '-') then ! Plus or minus sign:
            if (j == 1) then ! - leading unary operator ?
                res = .false.
            elseif (scan(F(j - 1:j - 1), '+-*/^(') > 0) then ! - other unary operator ?
                res = .false.
            elseif (scan(F(j + 1:j + 1), '0123456789') > 0 .and. & ! - in exponent of real number ?
                    scan(F(j - 1:j - 1), 'eEdD') > 0) then
                Dflag = .false.; Pflag = .false.
                k = j - 1
                do while (k > 1) !   step to the left in mantissa
                    k = k - 1
                    if (scan(F(k:k), '0123456789') > 0) then
                        Dflag = .true.
                    elseif (F(k:k) == '.') then
                        if (Pflag) then
                            exit !   * EXIT: 2nd appearance of '.'
                        else
                            Pflag = .true. !   * mark 1st appearance of '.'
                        end if
                    else
                        exit !   * all other characters
                    end if
                end do
                if (Dflag .and. (k == 1 .or. scan(F(k:k), '+-*/^(') > 0)) res = .false.
            end if
        end if
    end function IsBinaryOp

!*****************************************************************************************
    function RealNum(str, ibegin, inext, error) result(res)
        ! Get real number from string - Format: [blanks][+|-][nnn][.nnn][e|E|d|D[+|-]nnn]
        character(LEN=*), intent(in) :: str ! String
        real(rn)                       :: res ! Real number
        integer, optional, intent(out) :: ibegin, & ! Start position of real number
                                          inext ! 1st character after real number
        logical, optional, intent(out) :: error ! Error flag

        integer                        :: ib, in, istat
        logical                        :: Bflag, & ! .T. at begin of number in str
                                          InMan, & ! .T. in mantissa of number
                                          Pflag, & ! .T. after 1st '.' encountered
                                          Eflag, & ! .T. at exponent identifier 'eEdD'
                                          InExp, & ! .T. in exponent of number
                                          DInMan, & ! .T. if at least 1 digit in mant.
                                          DInExp, & ! .T. if at least 1 digit in exp.
                                          err ! Local error flag
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        Bflag = .true.; InMan = .false.; Pflag = .false.; Eflag = .false.; InExp = .false.
        DInMan = .false.; DInExp = .false.
        ib = 1
        in = 1
        do while (in <= len_trim(str))
            select case (str(in:in))
            case (' ') ! Only leading blanks permitted
                ib = ib + 1
                if (InMan .or. Eflag .or. InExp) exit
            case ('+', '-') ! Permitted only
                if (Bflag) then
                    InMan = .true.; Bflag = .false. ! - at beginning of mantissa
                elseif (Eflag) then
                    InExp = .true.; Eflag = .false. ! - at beginning of exponent
                else
                    exit ! - otherwise STOP
                end if
            case ('0':'9') ! Mark
                if (Bflag) then
                    InMan = .true.; Bflag = .false. ! - beginning of mantissa
                elseif (Eflag) then
                    InExp = .true.; Eflag = .false. ! - beginning of exponent
                end if
                if (InMan) DInMan = .true. ! Mantissa contains digit
                if (InExp) DInExp = .true. ! Exponent contains digit
            case ('.')
                if (Bflag) then
                    Pflag = .true. ! - mark 1st appearance of '.'
                    InMan = .true.; Bflag = .false. !   mark beginning of mantissa
                elseif (InMan .and. .not. Pflag) then
                    Pflag = .true. ! - mark 1st appearance of '.'
                else
                    exit ! - otherwise STOP
                end if
            case ('e', 'E', 'd', 'D') ! Permitted only
                if (InMan) then
                    Eflag = .true.; InMan = .false. ! - following mantissa
                else
                    exit ! - otherwise STOP
                end if
            case DEFAULT
                exit ! STOP at all other characters
            end select
            in = in + 1
        end do
        err = (ib > in - 1) .or. (.not. DInMan) .or. ((Eflag .or. InExp) .and. .not. DInExp)
        if (err) then
            res = 0.0_rn
        else
            read (str(ib:in - 1), *, IOSTAT=istat) res
            err = istat /= 0
        end if
        if (present(ibegin)) ibegin = ib
        if (present(inext)) inext = in
        if (present(error)) error = err
    end function RealNum

!*****************************************************************************************
    subroutine LowCase(str1, str2)
        ! Transform upper case letters in str1 into lower case letters, result is str2
        implicit none
        character(LEN=*), intent(in) :: str1
        character(LEN=*), intent(out) :: str2
        integer                        :: j, k
        character(LEN=*), parameter :: lc = 'abcdefghijklmnopqrstuvwxyz'
        character(LEN=*), parameter :: uc = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

        str2 = str1

        do j = 1, len_trim(str1)
            k = index(uc, str1(j:j))
            if (k > 0) str2(j:j) = lc(k:k)
        end do

    end subroutine LowCase

end module FortranParser
