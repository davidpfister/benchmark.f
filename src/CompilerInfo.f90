module benchmark_compilerinfo
    use, intrinsic :: iso_fortran_env, only : compiler_version, compiler_options
    use benchmark_systeminfo

    implicit none
    
    enum, bind(C)
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
    integer, parameter :: compiler_enum = kind(compiler_unknown)
    
    contains
    
    subroutine compilerinfo(os)
        integer, intent(in), optional :: os
        !private
        integer :: compiler, os_
        
        if (.not. present(os)) then
            os_ = get_os_type()
        else
            os_ = os
        end if
        
        compiler = get_compiler(os_)
        
        write(*,*) 'This file was compiled with ', &
                 compiler_version()
    end subroutine
    
    integer function get_compiler(os) result(id)
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
    
    logical function compiler_is_debug(compiler) result(is)
        integer, intent(in) :: compiler
        
        is = .false.
        select case (compiler)
            case (compiler_gcc)
        end select
    end function
end module