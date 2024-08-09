!> @ingroup group_benchmark
!> @defgroup group_options options
!> @details Base type for the benchmark runner
!> @{
module benchmark_options
    use benchmark_kinds
    
    implicit none
    
    !> @class runner_options
    !> @brief Provides a base class for the benchmark runner
    type, public :: runner_options
        
        integer,  public        :: maxcalls = 1000000       !< Maximum number of function calls
        integer,  public        :: csv_unit = 0             !< Integer designating the logical output unit for csv results. Null value corresponds to unset value
        real(r8), public        :: mintime = 100.0_r8       !< Minimum sampling time in ms to collect data
        real(r8), public        :: maxtime = 100000.0_r8    !< Maximum sampling time in ms to collect data
        real(r8), public        :: offset = 0.0_r8          !< Time offset corresponding to the methods overhead
        integer,  public        :: sampling_window = 20     !< Integer option to adjust the size of the sampling window
        real(r8), public        :: ssd_threshold = 0.05_r8  !<  Acceptance threshold for the steady-state detection
        logical, public         :: skip_prelude = .false.   !< Logical flag. If set to .true., only the benchmarking step will be performed
        !> @cond
        integer,  public        :: count = 0    
        !> @endcond            
        character(200), public  :: name = ''                !<  String name
    end type
    !> @}
end module
!> @}