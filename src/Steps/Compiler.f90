!https://github.com/fortran-lang/fpm/issues/50#issuecomment-731883071
!https://fortran-lang.discourse.group/t/compiler-flags-comparison/2692/2
!https://fortran-lang.discourse.group/t/retrieve-compiler-info-at-run-time/8192/19
!> @ingroup group_steps
!> @defgroup group_steps_compiler benchmark_steps_compiler
!! @brief Retrieves compiler information at run time as the compiler's name
!!          and version, as well as compilation options. It uses the intrinsic 
!!          functions `compiler_options` and `compiler_version` from the 
!!          module `iso_fortran_env`
!! @par
!! <h2>Remarks</h2>
!! The different compilers all come with there own macros, debug options and version information.
!! Some pieces of information are collected in the following table:
!!|compiler      | id                                  | outcome                                                                                                                         | macro                                         | debug                         |
!!|:------------:|:-----------------------------------:|:-------------------------------------------------------------------------------------------------------------------------------:|:---------------------------------------------:|:-----------------------------:|
!!|gfortran      | compiler_gcc                        | GCC version 14.1.0                                                                                                              |  `__GFORTRAN__`                               | -O0, -g[n]                    |
!!|f95           | compiler_f95                        |                                                                                                                                 |                                               |                               |
!!|caf           | compiler_caf                        | GCC version 12.3.0                                                                                                              |                                               |                               |
!!|ifort         | compiler_intel_classic_nix          | Intel(R) Fortran Intel(R) 64 Compiler Classic for applications running on Intel(R) 64, Version 2021.12.0 Build 20240211_000000  | `__INTEL_COMPILER`                            | -g[n], -debug, -O0            |
!!|ifort         | compiler_intel_classic_mac          |                                                                                                                                 | `__INTEL_COMPILER`                            | -g[n], -debug, -O0            |
!!|ifort         | compiler_intel_classic_windows (x64)| Intel(R) Fortran Intel(R) 64 Compiler Classic for applications running on Intel(R) 64, Version 2021.3.0 Build 20210609_000000   | `__INTEL_COMPILER`                            | /Zi, /Z7, /debug /Od          |
!!|ia32          | compiler_intel_classic_windows (x86)| Intel(R) Fortran Intel(R) 64 Compiler Classic for applications running on IA-32, Version 2021.12.0 Build 20240222_000000        | `__INTEL_COMPILER`                            | /Zi, /Z7, /debug:full /Od     |
!!|ifx           | compiler_intel_llvm_nix             | Intel(R) Fortran Compiler for applications running on Intel(R) 64, Version 2024.1.2 Build 20240508                              | `__INTEL_COMPILER` and `__INTEL_LLVM_COMPILER`| /Zi, /Z7, /debug:full /Od     |
!!|ifx           | compiler_intel_llvm_windows         | Intel(R) Fortran Compiler for applications running on Intel(R) 64, Version 2024.1.0 Build 20240308                              | `__INTEL_COMPILER` and `__INTEL_LLVM_COMPILER`| /Zi, /Z7, /debug:full /Od     |
!!|pgfortran     | compiler_pgi                        |                                                                                                                                 | `__PGI`                                       | -O0, -g (not -gopt)           |
!!|nvfortran     | compiler_nvhpc                      | nvfortran 21.5-0 LLVM                                                                                                           | `__NVCOMPILER` (and `__PGI` for some versions)| -O0, -g[n]                    |
!!|nagfor        | compiler_nag                        | NAG Fortran Compiler Release 7.2(Shin-Urayasu) Build 7205                                                                       | `NAGFOR`                                      | -O0, -g[n]                    |
!!|flang         | compiler_flang                      |                                                                                                                                 | `__PGLLVM__`                                  | -O0, -g[n]                    |
!!|flang         | compiler_flang_new                  | flang version 19.0.0                                                                                                            | `__flang__`                                   | -O0, -g[n]                    |
!!|f18           | compiler_f18                        |                                                                                                                                 |                                               |                               |
!!|xlf90         | compiler_ibmxl                      | IBM XL Fortran for AIX, 16.1 (5765-J14, 5725-C74) Version: 16.01.0000.0000                                                      | `__IBMC__`                                    | -qnoopt, -O0, -g              |
!!|crayftn       | compiler_cray                       |                                                                                                                                 | `_CRAYFTN`                                    | -O0, -g[n]                    |
!!|lf95          | compiler_lahey                      | Lahey/Fujitsu Fortran 95 Compiler Release L6.10a                                                                                | `__COMPILER_LAHEY`                            | -O0, -g                       |
!!|lfortran      | compiler_lfortran                   | LFortran version 0.36.0                                                                                                         |                                               |                               |
!! @{
module benchmark_steps_compiler
    use, intrinsic :: iso_fortran_env, only : compiler_version, compiler_options
    use benchmark_workflow, only: workflow
    use benchmark_output_unit
    use benchmark_systeminfo
    use benchmark_warning
    
    implicit none
    
    private
    
    public :: compiler

    enum, bind(c)
    enumerator :: &
        COMPILER_UNKNOWN, &
        COMPILER_GCC, &
        COMPILER_F95, &
        COMPILER_CAF, &
        COMPILER_INTEL_CLASSIC_NIX, &
        COMPILER_INTEL_CLASSIC_MAC, &
        COMPILER_INTEL_CLASSIC_WINDOWS, &
        COMPILER_INTEL_LLVM_NIX, &
        COMPILER_INTEL_LLVM_WINDOWS, &
        COMPILER_INTEL_LLVM_UNKNOWN, &
        COMPILER_PGI, &
        COMPILER_NVHPC, &
        COMPILER_NAG, &
        COMPILER_FLANG, &
        COMPILER_FLANG_NEW, &
        COMPILER_F18, &
        COMPILER_IBMXL, &
        COMPILER_CRAY, &
        COMPILER_LAHEY, &
        COMPILER_LFORTRAN
    end enum

    integer, parameter, public :: COMPILER_ENUM = kind(COMPILER_UNKNOWN) !< Compiler enum type
    

    !> @class compilerinfo
    !! @brief Provides base class for storing compiler information
    !! @verbatim type, public :: compilerinfo @endverbatim
    !! @par
    !! <h2>Constructors</h2>
    !! Initializes a new instance of the @ref compilerinfo class
    !! <h3>compilerinfo(integer(COMPILER_ENUM), character(20), character(20), character(20))</h3>
    !! @verbatim type(compilerinfo) :: ci@endverbatim
    !! 
    !! @b Examples
    !! ```fortran
    !! use benchmark_steps_compiler
    !!
    !!  type(compilerinfo) :: ci
    !! ```
    !! @b Remarks
    type, public :: compilerinfo
        integer(COMPILER_ENUM) :: id        !< Compiler ID
        character(20)          :: name      !< Compiler name
        character(20)          :: vendor    !< Compiler vendor
        character(20)          :: version   !< Compiler release version
    end type
    
    !> @interface compiler
    !! @ingroup group_steps_compiler
    !! @ingroup group_steps_compiler
    !! @brief 
    !! @verbatim interface compiler @endverbatim
    !! @par
    !! <h2>Constructors</h2>
    !! Initializes a new instance of the @ref compilerinfo class
    !! <h3>compilerinfo(integer(COMPILER_ENUM), character(20), character(20), character(20))</h3>
    !! @verbatim type(compilerinfo) :: ci@endverbatim
    !! 
    !! @b Examples
    !! ```fortran
    !! use benchmark_steps_compiler
    !!
    !!  type(compilerinfo) :: ci
    !! ```
    !! @b Remarks
    interface compiler
        !! @cond
        module procedure :: compiler_new
        !> @endcond
    end interface
    
    contains
    
    type(workflow) function compiler_new() result(step)
        step%header = '                                     -- COMPILER INFO --                                               '
        step%action => step_run
    end function
    
    subroutine step_run(step)
        class(workflow), intent(inout) :: step
        
        write (output_unit, '(A)') new_line('A'), step%header
        call get_compilerinfo()
    end subroutine

    subroutine get_compilerinfo(os)
        integer, intent(in), optional :: os
        !private
        integer                     :: compiler, os_, i, j, k, l
        character(:), allocatable   :: version
        character(:), allocatable   :: options
        type(compilerinfo)          :: ci
        
        if (.not. present(os)) then
            os_ = get_os_type()
        else
            os_ = os
        end if
        
        version = to_lower(compiler_version())
        
        if (index(version, 'gcc') > 0) then
            compiler = COMPILER_GCC
        else if (index(version, 'intel') > 0) then
            if (index(version, 'classic') > 0) then
                if (os_ == OS_LINUX) then
                    compiler = COMPILER_INTEL_CLASSIC_NIX
                elseif (os_ == OS_WINDOWS) then
                    compiler = COMPILER_INTEL_CLASSIC_WINDOWS
                elseif (os_ == OS_MACOS) then
                    compiler = COMPILER_INTEL_CLASSIC_MAC
                end if
            else
                if (os_ == OS_LINUX) then
                    compiler = COMPILER_INTEL_LLVM_NIX
                elseif (os_ == OS_WINDOWS) then
                    compiler = COMPILER_INTEL_LLVM_WINDOWS
                else
                    compiler = COMPILER_INTEL_LLVM_UNKNOWN
                end if
            end if
        else if (index(version, 'nvfortran') > 0) then
            compiler = COMPILER_NVHPC
        else if (index(version, 'nag fortran') > 0) then
            compiler = COMPILER_NAG
        else if (index(version, 'flang') > 0) then
            compiler = COMPILER_FLANG
        else if (index(version, 'ibm xl') > 0) then
            compiler = COMPILER_IBMXL
        else if (index(version, 'lahey/fujitsu') > 0) then
            compiler = COMPILER_LAHEY
        else if (index(version, 'lfortran') > 0) then
            compiler = COMPILER_LFORTRAN
        else
            compiler = get_compiler_at_compiletime(os_)
        end if
        
        ci = build_compilerinfo(compiler, version)
        options = compiler_options()
        i = 1
        k = 1
        write(output_unit,'(A)') '                           Name:                      '//trim(ci%name)
        write(output_unit,'(A)') '                           Vendor:                    '//trim(ci%vendor)
        write(output_unit,'(A)') '                           Version:                   '//trim(ci%version)
        do while (i < len(options))
            k = index(options(i:), ' ')
            if (k > 0) then
                do while (k < 60)
                    if (j == i + k) exit
                    j = i + k
                    l = index(options(i + k + 1:), ' ')
                    if (l > 0) then
                        k = k + l
                    else 
                        k = k + len(options(i + k + 1:))
                        j = i + k
                        exit
                    end if
                end do
                if (i == j + 1) j = i + 60
            else
                j = len(options)
            end if

            if (i == 1) then
                write(output_unit,'(A)') '                           Options:                   '//options(i:j)
            else
                write(output_unit,'(A)') '                                                      '//options(i:j)
            end if
            i = j + 1
            k = 0
        end do
        
        associate(isdebug => compiler_is_debug(compiler, options))
            write(output_unit,'(A)') '                           Profile:                   '//trim(merge('Debug  ', 'Release', isdebug))
            if (isdebug) then 
                call warning_debug()
            end if
        end associate
        
    contains
    
        pure function to_lower(input) result(str)
            character(*), intent(in) :: input !< the input to be lowercased
            character(len(input))    :: str !< the lower case input
            !private
            integer, parameter :: change_case = 32
            integer :: i

            do i = 1, len(input)
                select case (input(i:i))
                case ('A':'Z')
                    str(i:i) = char(ichar(input(i:i)) + change_case)
                case default
                    str(i:i) = input(i:i)
                end select
            end do
        end function
    end subroutine
    
    integer function get_compiler_at_compiletime(os) result(id)
        integer, intent(in) :: os
        
        id = COMPILER_UNKNOWN
#ifdef __GFORTRAN__
        id = COMPILER_GCC
#elif defined(__INTEL_COMPILER)
#if defined(__INTEL_LLVM_COMPILER)
        if (os == OS_LINUX) then
            id = COMPILER_INTEL_LLVM_NIX
        elseif (os == OS_WINDOWS) then
            id = COMPILER_INTEL_LLVM_WINDOWS
        else
            id = COMPILER_INTEL_LLVM_UNKNOWN
        end if
#else
        if (os == OS_LINUX) then
            id = COMPILER_INTEL_CLASSIC_NIX
        elseif (os == OS_WINDOWS) then
            id = COMPILER_INTEL_CLASSIC_WINDOWS
        elseif (os == OS_MACOS) then
            id = COMPILER_INTEL_CLASSIC_MAC
        end if
#endif
#elif defined (NAGFOR)
        id = COMPILER_NAG
#elif defined (__PGI)
#if defined (__NVCOMPILER]
        id = COMPILER_NVHPC
#else
        id = COMPILER_PGI
#endif
#elif defined (__NVCOMPILER)
        id = COMPILER_NVHPC
#elif defined (_CRAYFTN)
        id = COMPILER_CRAY
#elif defined (__IBMC__]
        id = COMPILER_IBMXL
#endif
    end function
    
    logical function compiler_is_debug(compiler, options) result(is)
        integer, intent(in)      :: compiler
        character(*), intent(in) :: options
        
        is = .false.
        if (index(options, '-O0') > 0) then
            is = .true.
            return 
        end if
        select case (compiler)
        case (COMPILER_IBMXL)
            if (index(options, '-qnoopt') > 0) then
                is = .true.
                return 
            end if
        case (COMPILER_INTEL_CLASSIC_NIX, COMPILER_INTEL_CLASSIC_MAC, COMPILER_INTEL_LLVM_NIX, COMPILER_INTEL_LLVM_UNKNOWN)
            if (index(options, '-debug') > 0) then
                is = .true.
                return 
            end if
        case (COMPILER_INTEL_CLASSIC_WINDOWS, COMPILER_INTEL_LLVM_WINDOWS)
            if (index(options, '/debug') > 0 .or. index(options, '/Od') > 0) then
                is = .true.
                return 
            end if
        case (COMPILER_LFORTRAN)
            if (index(options, '--debug-with-line-column')> 0) then
                is = .true.
                return 
            end if
        end select
    end function
    
    type(compilerinfo) function build_compilerinfo(id, string) result(info)
        integer, intent(in)      :: id
        character(*), intent(in) :: string
        !private
        integer :: i, j
        
        select case (id)
        case (COMPILER_UNKNOWN)
            info = compilerinfo(id, 'UNKNOWN', 'UNKNOWN', 'X.X.X.X')
        case (COMPILER_GCC)
            i = index(string, 'version')
            info = compilerinfo(id, 'gfortran', 'GCC', trim(adjustl(string(i+7:))))
        case (COMPILER_F95)
            info = compilerinfo(id, 'f95', 'ORACLE', 'X.X.X.X')
        case (COMPILER_CAF)
            info = compilerinfo(id, 'caf', 'OPENCOARRAY', 'X.X.X.X')
        case (COMPILER_INTEL_CLASSIC_NIX)
            i = index(string, 'version')
            j = index(string, 'build')
            info = compilerinfo(id, 'ifort', 'INTEL', trim(adjustl(string(i+7:j-1))))
        case (COMPILER_INTEL_CLASSIC_MAC)
            i = index(string, 'version')
            j = index(string, 'build')
            info = compilerinfo(id, 'ifort','INTEL', trim(adjustl(string(i+7:j-1))))
        case (COMPILER_INTEL_CLASSIC_WINDOWS)
            i = index(string, 'version')
            j = index(string, 'build')
            info = compilerinfo(id, merge('ia32 ', 'ifort', index(string, 'IA-32') > 0),'INTEL', trim(adjustl(string(i+7:j-1))))
        case (COMPILER_INTEL_LLVM_NIX)
            i = index(string, 'version')
            j = index(string, 'build')
            info = compilerinfo(id, 'ifx', 'INTEL', trim(adjustl(string(i+7:j-1))))
        case (COMPILER_INTEL_LLVM_WINDOWS)
            i = index(string, 'version')
            j = index(string, 'build')
            info = compilerinfo(id, 'ifx', 'INTEL', trim(adjustl(string(i+7:j-1))))
        case (COMPILER_INTEL_LLVM_UNKNOWN)
            i = index(string, 'version')
            j = index(string, 'build')
            info = compilerinfo(id, 'ifx', 'INTEL', trim(adjustl(string(i+7:j-1))))
        case (COMPILER_PGI)
            info = compilerinfo(id, 'pgfortran','PGI', 'X.X.X.X')
        case (COMPILER_NVHPC)
            i = index(string, 'nvfortran')
            j = index(string, 'llvm')
            info = compilerinfo(id, 'nvhpc','NVIDIA', trim(adjustl(string(i+9:j-1))))
        case (COMPILER_NAG)
            i = index(string, 'release')
            j = index(string, '(')
            info = compilerinfo(id, 'nagfor', 'NAG', trim(adjustl(string(i+7:j-1))))
        case (COMPILER_FLANG)
            i = index(string, 'version')
            info = compilerinfo(id, 'flang', 'FLANG', trim(adjustl(string(i+7:))))
        case (COMPILER_FLANG_NEW)
            i = index(string, 'version')
            info = compilerinfo(id, 'flang', 'FLANG', trim(adjustl(string(i+7:))))
        case (COMPILER_F18)
            info = compilerinfo(id, 'f18', 'F18', 'X.X.X.X')
        case (COMPILER_IBMXL)
            i = index(string, 'version:')
            info = compilerinfo(id, 'xlf90', 'IBM', trim(adjustl(string(i+8:))))
        case (COMPILER_CRAY)
            info = compilerinfo(id, 'crayftn','CRAY', 'X.X.X.X')
        case (COMPILER_LAHEY)
            i = index(string, 'release')
            info = compilerinfo(id, 'lf95', 'LAHEY/FUJITSU', trim(adjustl(string(i+7:))))
        case (COMPILER_LFORTRAN)
            i = index(string, 'version')
            info = compilerinfo(id, 'lfortran', 'LFORTRAN', trim(adjustl(string(i+7:))))
        case default
            info = compilerinfo(id, 'UNKNOWN', 'UNKNOWN', 'X.X.X.X')
        end select
    end function
end module
!! @}