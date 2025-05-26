!> @defgroup group_options benchmark_options
!! @brief Options module
module benchmark_options
    use benchmark_kinds
    
    implicit none
    
    !> @class runner_options
    !! @ingroup group_options
    !! @brief Provides a base class for the benchmark runner
    !! @par
    !! <h2>Examples</h2>
    !! The first example demonstrates how to instantiate the @ref runner_options
    !! type and modify some values.
    !! @n@n
    !! @snippet snippet.f90 options_constructor
    !! @n 
    !! The second example shows how to export benchmark results to csv file
    !! @snippet snippet.f90 options_csv
    !! @par
    !! <h2>Remarks</h2>
    !! The @ref runner_options is the base class for the @link benchmark_library::runner runner @endlink class. 
    type, public :: runner_options
        integer,  public        :: maxcalls = 100000000     !< Maximum number of function calls
        integer,  public        :: csv_unit = 0             !< Integer designating the logical output unit for csv results. Null value corresponds to unset value
        real(r8), public        :: mintime = 100.0_r8       !< Minimum sampling time in ms to collect data
        real(r8), public        :: maxtime = 100000.0_r8    !< Maximum sampling time in ms to collect data
        real(r8), public        :: overhead = 0.0_r8        !< Time overhead corresponding to the surounding methods calls
        integer,  public        :: sampling_window = 20     !< Integer option to adjust the size of the sampling window
        real(r8), public        :: ssd_threshold = 0.05_r8  !<  Acceptance threshold for the steady-state detection
        logical, public         :: skip_prelude = .false.   !< Logical flag. If set to .true., only the benchmarking step will be performed
        !> @cond
        integer,  public        :: count = 0    
        !> @endcond            
        character(200), public  :: name = ''                !<  String name
    end type
end module
!! @}