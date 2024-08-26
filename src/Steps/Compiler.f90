!https://github.com/fortran-lang/fpm/issues/50#issuecomment-731883071
!https://fortran-lang.discourse.group/t/compiler-flags-comparison/2692/2
!https://fortran-lang.discourse.group/t/retrieve-compiler-info-at-run-time/8192/19
!> @ingroup group_steps
!> @defgroup group_steps_compiler benchmark_steps_compiler
!! @details Retrieves compiler information at run time as the compiler's name
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
!> @{
module benchmark_steps_compiler
    use, intrinsic :: iso_fortran_env, only : compiler_version, compiler_options
    use benchmark_workflow, only: workflow
    use benchmark_output_unit
    use benchmark_systeminfo
    use benchmark_warning
    
    implicit none
    
    private
    
    public :: compiler

    !> @name Enums
    !! @details
    !! compiler_unknown
    !! compiler_gcc
    !! compiler_f95
    !! compiler_caf
    !! compiler_intel_classic_nix
    !! compiler_intel_classic_mac
    !! compiler_intel_classic_windows
    !! compiler_intel_llvm_nix
    !! compiler_intel_llvm_windows
    !! compiler_intel_llvm_unknown
    !! compiler_pgi
    !! compiler_nvhpc
    !! compiler_nag
    !! compiler_flang
    !! compiler_flang_new
    !! compiler_f18
    !! compiler_ibmxl
    !! compiler_cray
    !! compiler_lahey
    !! compiler_lfortran
    !! @{
    enum, bind(c)
    enumerator :: &
        compiler_unknown, &
        compiler_gcc, &
        compiler_f95, &
        compiler_caf, &
        compiler_intel_classic_nix, &
        compiler_intel_classic_mac, &
        compiler_intel_classic_windows, &
        compiler_intel_llvm_nix, &
        compiler_intel_llvm_windows, &
        compiler_intel_llvm_unknown, &
        compiler_pgi, &
        compiler_nvhpc, &
        compiler_nag, &
        compiler_flang, &
        compiler_flang_new, &
        compiler_f18, &
        compiler_ibmxl, &
        compiler_cray, &
        compiler_lahey, &
        compiler_lfortran
    end enum
    
    integer, parameter, public :: compiler_enum = kind(compiler_unknown)
    !> @}

    !> @class compilerinfo
    !! @ingroup group_steps_compiler
    !! @brief 
    !! @verbatim type, public :: compilerinfo @endverbatim
    !! <h2>Examples</h2>
    !! <h2>Remarks</h2>
    !! @par
    !! <h2>Constructors</h2>
    !! Initializes a new instance of the @ref compilerinfo class
    !! <h3>compilerinfo(integer(compiler_enum), character(20), character(20), character(20))</h3>
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
        integer(compiler_enum) :: id
        character(20)          :: name
        character(20)          :: vendor
        character(20)          :: version
    end type
    
    !> @interface compiler
    !! @ingroup group_steps_compiler
    !! @brief 
    !! @verbatim interface compiler @endverbatim
    !! <h2>Examples</h2>
    !! <h2>Remarks</h2>
    !! @par
    !! <h2>Constructors</h2>
    !! Initializes a new instance of the @ref compilerinfo class
    !! <h3>compilerinfo(integer(compiler_enum), character(20), character(20), character(20))</h3>
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
            compiler = compiler_gcc
        else if (index(version, 'intel') > 0) then
            if (index(version, 'classic') > 0) then
                if (os_ == OS_LINUX) then
                    compiler = compiler_intel_classic_nix
                elseif (os_ == OS_WINDOWS) then
                    compiler = compiler_intel_classic_windows
                elseif (os_ == OS_MACOS) then
                    compiler = compiler_intel_classic_mac
                end if
            else
                if (os_ == OS_LINUX) then
                    compiler = compiler_intel_llvm_nix
                elseif (os_ == OS_WINDOWS) then
                    compiler = compiler_intel_llvm_windows
                else
                    compiler = compiler_intel_llvm_unknown
                end if
            end if
        else if (index(version, 'nvfortran') > 0) then
            compiler = compiler_nvhpc
        else if (index(version, 'nag fortran') > 0) then
            compiler = compiler_nag
        else if (index(version, 'flang') > 0) then
            compiler = compiler_flang
        else if (index(version, 'ibm xl') > 0) then
            compiler = compiler_ibmxl
        else if (index(version, 'lahey/fujitsu') > 0) then
            compiler = compiler_lahey
        else if (index(version, 'lfortran') > 0) then
            compiler = compiler_lfortran
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
        
        id = compiler_unknown
#ifdef __GFORTRAN__
        id = compiler_gcc
#elif defined(__INTEL_COMPILER)
#if defined(__INTEL_LLVM_COMPILER)
        if (os == OS_LINUX) then
            id = compiler_intel_llvm_nix
        elseif (os == OS_WINDOWS) then
            id = compiler_intel_llvm_windows
        else
            id = compiler_intel_llvm_unknown
        end if
#else
        if (os == OS_LINUX) then
            id = compiler_intel_classic_nix
        elseif (os == OS_WINDOWS) then
            id = compiler_intel_classic_windows
        elseif (os == OS_MACOS) then
            id = compiler_intel_classic_mac
        end if
#endif
#elif defined (NAGFOR)
        id = compiler_nag
#elif defined (__PGI)
#if defined (__NVCOMPILER]
        id = compiler_nvhpc
#else
        id = compiler_pgi
#endif
#elif defined (__NVCOMPILER)
        id = compiler_nvhpc
#elif defined (_CRAYFTN)
        id = compiler_cray
#elif defined (__IBMC__]
        id = compiler_ibmxl
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
        case (compiler_ibmxl)
            if (index(options, '-qnoopt') > 0) then
                is = .true.
                return 
            end if
        case (compiler_intel_classic_nix, compiler_intel_classic_mac, compiler_intel_llvm_nix, compiler_intel_llvm_unknown)
            if (index(options, '-debug') > 0) then
                is = .true.
                return 
            end if
        case (compiler_intel_classic_windows, compiler_intel_llvm_windows)
            if (index(options, '/debug') > 0 .or. index(options, '/Od') > 0) then
                is = .true.
                return 
            end if
        case (compiler_lfortran)
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
        case (compiler_unknown)
            info = compilerinfo(id, 'UNKNOWN', 'UNKNOWN', 'X.X.X.X')
        case (compiler_gcc)
            i = index(string, 'version')
            info = compilerinfo(id, 'gfortran', 'GCC', trim(adjustl(string(i+7:))))
        case (compiler_f95)
            info = compilerinfo(id, 'f95', 'ORACLE', 'X.X.X.X')
        case (compiler_caf)
            info = compilerinfo(id, 'caf', 'OPENCOARRAY', 'X.X.X.X')
        case (compiler_intel_classic_nix)
            i = index(string, 'version')
            j = index(string, 'build')
            info = compilerinfo(id, 'ifort', 'INTEL', trim(adjustl(string(i+7:j-1))))
        case (compiler_intel_classic_mac)
            i = index(string, 'version')
            j = index(string, 'build')
            info = compilerinfo(id, 'ifort','INTEL', trim(adjustl(string(i+7:j-1))))
        case (compiler_intel_classic_windows)
            i = index(string, 'version')
            j = index(string, 'build')
            info = compilerinfo(id, merge('ia32 ', 'ifort', index(string, 'IA-32') > 0),'INTEL', trim(adjustl(string(i+7:j-1))))
        case (compiler_intel_llvm_nix)
            i = index(string, 'version')
            j = index(string, 'build')
            info = compilerinfo(id, 'ifx', 'INTEL', trim(adjustl(string(i+7:j-1))))
        case (compiler_intel_llvm_windows)
            i = index(string, 'version')
            j = index(string, 'build')
            info = compilerinfo(id, 'ifx', 'INTEL', trim(adjustl(string(i+7:j-1))))
        case (compiler_intel_llvm_unknown)
            i = index(string, 'version')
            j = index(string, 'build')
            info = compilerinfo(id, 'ifx', 'INTEL', trim(adjustl(string(i+7:j-1))))
        case (compiler_pgi)
            info = compilerinfo(id, 'pgfortran','PGI', 'X.X.X.X')
        case (compiler_nvhpc)
            i = index(string, 'nvfortran')
            j = index(string, 'llvm')
            info = compilerinfo(id, 'nvhpc','NVIDIA', trim(adjustl(string(i+9:j-1))))
        case (compiler_nag)
            i = index(string, 'release')
            j = index(string, '(')
            info = compilerinfo(id, 'nagfor', 'NAG', trim(adjustl(string(i+7:j-1))))
        case (compiler_flang)
            i = index(string, 'version')
            info = compilerinfo(id, 'flang', 'FLANG', trim(adjustl(string(i+7:))))
        case (compiler_flang_new)
            i = index(string, 'version')
            info = compilerinfo(id, 'flang', 'FLANG', trim(adjustl(string(i+7:))))
        case (compiler_f18)
            info = compilerinfo(id, 'f18', 'F18', 'X.X.X.X')
        case (compiler_ibmxl)
            i = index(string, 'version:')
            info = compilerinfo(id, 'xlf90', 'IBM', trim(adjustl(string(i+8:))))
        case (compiler_cray)
            info = compilerinfo(id, 'crayftn','CRAY', 'X.X.X.X')
        case (compiler_lahey)
            i = index(string, 'release')
            info = compilerinfo(id, 'lf95', 'LAHEY/FUJITSU', trim(adjustl(string(i+7:))))
        case (compiler_lfortran)
            i = index(string, 'version')
            info = compilerinfo(id, 'lfortran', 'LFORTRAN', trim(adjustl(string(i+7:))))
        case default
            info = compilerinfo(id, 'UNKNOWN', 'UNKNOWN', 'X.X.X.X')
        end select
    end function
end module