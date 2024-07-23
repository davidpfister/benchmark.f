module benchmark_systeminfo
    use benchmark_output_unit
    use benchmark_output_unit
    
    implicit none
    
    enum, bind(c)
        enumerator :: OS_UNKNOWN = 0
        enumerator :: OS_LINUX   = 1
        enumerator :: OS_MACOS   = 2
        enumerator :: OS_WINDOWS = 3
        enumerator :: OS_CYGWIN  = 4
        enumerator :: OS_SOLARIS = 5
        enumerator :: OS_FREEBSD = 6
        enumerator :: OS_OPENBSD = 7
    end enum
    
    contains
    
    subroutine get_systeminfo()
        !private
        character(:), allocatable   :: tmpout, output, cmsg
        character(:), allocatable   :: cmd
        integer :: os, lu, ios
        integer :: ierr,  cstat
        character(200) :: line
        logical :: exists
        
        allocate(character(256) :: cmsg)
        
        tmpout = 'info.sys'
        inquire(file=tmpout, exist=exists)
        
        if (.not. exists) then
            open(newunit = lu, status='new', file = tmpout); close(lu)
            output = " 1> "//tmpout
            ierr = 0
        
            os = get_os_type()
            ! Define the shell command.
            if (os == OS_MACOS) then
                cmd = 'uname -a '//output//'; sysctl -a | grep machdep.cpu '//output//'; system_profiler SPHardwareDataType '//output
            elseif (os == OS_LINUX) then
                cmd = 'uname -a '//output//'; lscpu | grep -v "Not affected" | grep -v "Flags"'//output
            elseif (os == OS_WINDOWS) then
                cmd = 'systeminfo | find /V /I "hotfix" | find /V /I "network"' //&
                    '| find /V "Connection Name" | find /V "[" | find /V "DHCP" ' //&
                    '| find /V "Status" | find /V "IP address" | find /V "Hyper-V" ' // output
            else
                return
            end if

            call execute_command_line(cmd, wait=.true., exitstat=ierr, cmdstat=cstat, cmdmsg=cmsg)
            if (ierr /= 0) then
                write (output_unit, *) cmd
                write (output_unit, *) 'exitstat: ', ierr
                write (output_unit, *) 'cmdstat:  ', cstat
                write (output_unit, *) 'cmdmsg:   ', cmsg
                return
            end if
        end if
        
        open(newunit = lu, status='old', file = tmpout)
        do
            read(lu, '(A)', iostat=ios) line
            if (ios /= 0) exit
            if (len_trim(line) > 0) write(output_unit, '(A)') '                           '//trim(line)
        end do

        !close(lu, status = 'delete')
    end subroutine
    
    !> @brief Returns the OS type.
    !> @details At first, the environment variable `OS` is checked, which is usually
    !! found on Windows. Then, `OSTYPE` is read in and compared with common
    !! names. If this fails too, check the existence of files that can be
    !! found on specific system types only.
    !> @returns an integer. Returns OS_UNKNOWN if the operating system cannot be determined.
    integer function get_os_type() result(r)
        !private        
        character(len=255)          :: val
        integer                     :: length, rc
        logical                     :: file_exists
        integer, allocatable, save  :: ret
        !$omp threadprivate(ret, first_run)

        if (allocated(ret)) then
            r = ret
            return
        end if
        r = OS_UNKNOWN

        ! Check environment variable `OSTYPE`.
        call get_environment_variable('OSTYPE', val, length, rc)

        if (rc == 0 .and. length > 0) then
            ! Linux
            if (index(val, 'linux') > 0) then
                r = OS_LINUX
                ret = r
                return
            end if

            ! macOS
            if (index(val, 'darwin') > 0) then
                r = OS_MACOS
                ret = r
                return
            end if

            ! Windows, MSYS, MinGW, Git Bash
            if (index(val, 'win') > 0 .or. index(val, 'msys') > 0) then
                r = OS_WINDOWS
                ret = r
                return
            end if

            ! Cygwin
            if (index(val, 'cygwin') > 0) then
                r = OS_CYGWIN
                ret = r
                return
            end if

            ! Solaris, OpenIndiana, ...
            if (index(val, 'SunOS') > 0 .or. index(val, 'solaris') > 0) then
                r = OS_SOLARIS
                ret = r
                return
            end if

            ! FreeBSD
            if (index(val, 'FreeBSD') > 0 .or. index(val, 'freebsd') > 0) then
                r = OS_FREEBSD
                ret = r
                return
            end if

            ! OpenBSD
            if (index(val, 'OpenBSD') > 0 .or. index(val, 'openbsd') > 0) then
                r = OS_OPENBSD
                ret = r
                return
            end if
        end if

        ! Check environment variable `OS`.
        call get_environment_variable('OS', val, length, rc)

        if (rc == 0 .and. length > 0 .and. index(val, 'Windows_NT') > 0) then
            r = OS_WINDOWS
            ret = r
            return
        end if

        ! Linux
        inquire (file='/etc/os-release', exist=file_exists)

        if (file_exists) then
            r = OS_LINUX
            ret = r
            return
        end if

        ! macOS
        inquire (file='/usr/bin/sw_vers', exist=file_exists)

        if (file_exists) then
            r = OS_MACOS
            ret = r
            return
        end if

        ! FreeBSD
        inquire (file='/bin/freebsd-version', exist=file_exists)

        if (file_exists) then
            r = OS_FREEBSD
            ret = r
            return
        end if
    end function
    
    pure function os_name(os)
        integer, intent(in) :: os
        !private
        character(:), allocatable :: OS_name

        select case (os)
            case (OS_LINUX);   OS_NAME =  "Linux"
            case (OS_MACOS);   OS_NAME =  "macOS"
            case (OS_WINDOWS); OS_NAME =  "Windows"
            case (OS_CYGWIN);  OS_NAME =  "Cygwin"
            case (OS_SOLARIS); OS_NAME =  "Solaris"
            case (OS_FREEBSD); OS_NAME =  "FreeBSD"
            case (OS_OPENBSD); OS_NAME =  "OpenBSD"
            case (OS_UNKNOWN); OS_NAME =  "Unknown"
            case default     ; OS_NAME =  "Unknown"
        end select
    end function
end module