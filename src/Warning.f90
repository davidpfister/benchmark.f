!> @file
!! @defgroup group_warning Warning
!! Provides methods for displaying warning messages.
!! @par
!! <h2>Examples</h2>
!! The following example demonstrates some of the methods found in the
!! @link benchmark_warning benchmark_warning @endlink module.
!! @n@n
!! The first example shows how to use the method @link benchmark_warning::warning_debug warning_debug @endlink.
!! @snippet snippet.f90 warning_example1
!! The output message is the following
!! @code{.txt}
!! <!>     WARNING     <!>
!! DEBUG profile detected. The results might be unreliable.
!! @endcode
!! The second example shows how to use the method @link benchmark_warning::warning_maxcalls warning_maxcalls @endlink.
!! @snippet snippet.f90 warning_example2
!! The output message is the following
!! @code{.txt}
!! <!>     WARNING     <!>
!! Maximum number of calls reached for some cases.  Steady state may not have been reached for all runs.
!! @endcode
!! @par
!! <h2>Remarks</h2>
!! The call to the subroutine @link benchmark_warning::warning_maxcalls warning_maxcalls @endlink is bound to
!! the value of the logical variable @link benchmark_warning::display_maxcall_warning display_maxcall_warning @endlink.
!! The use if that switch prevents calling the same warning multiple times when
!! used in a loop.
module benchmark_warning
    use benchmark_kinds
    use benchmark_output_unit

    implicit none; private

    public :: display_maxcall_warning, &
            warning_debug, &
            warning_maxcalls

    !> @brief Switch to control the display of the warning message
    !! @ingroup group_warning
    logical :: display_maxcall_warning = .false.

contains

    !> Warning triggered when a DEBUG profile is detected.
    !!
    !! @ingroup group_warning
    !! @b Remarks
    subroutine warning_debug()
#ifndef _QUIET
        write(output_unit, '(A)') new_line('A'), &
                &                '                                  <!>     WARNING     <!>'
        write(output_unit, '(A)')                     '                           DEBUG profile detected. The results' //&
                ' might be unreliable.'
        flush(output_unit)
#endif
    end subroutine

    !> Warning triggered when the maximum number of function calls
    !! is reached. The maximum value can be set in
    !! @link benchmark_options::runner_options runner_options @endlink
    !!
    !! @ingroup group_warning
    !! @b Remarks
    subroutine warning_maxcalls()
#ifndef _QUIET
        write(output_unit, '(A)') new_line('A'), &
                &                '                                  <!>     WARNING     <!>'
        write(output_unit, '(A)') '                           Maximum number of calls reached for some cases. Steady state' //&
                ' may not have been reached for all runs.'
        flush(output_unit)
#endif
    end subroutine
end module
