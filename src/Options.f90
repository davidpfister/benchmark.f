!> @ingroup group_all group_benchmark
!> @author davidpfister
module benchmark_options
    use benchmark_kinds
    
    implicit none
    
    type, public :: runner_options
        integer, public        :: maxcalls = 1000000 !< maximum number of function calls
        integer, public        :: csv_unit = -1
        real(r8), public       :: mintime = 100.0_r8 !< minimum sampling time in ms to collect data
        real(r8), public       :: maxtime = 100000.0_r8 !< maximum sampling time in ms to collect data
        real(r8), public       :: offset = 0.0_r8 !< maximum sampling time in ms to collect data
        integer, public        :: sampling_window = 20 !< option to adjust the size of the sampling window
        real(r8), public       :: ssd_threshold = 0.05_r8 !< acceptance threshold for the steady-state detection
        !private
        integer, public        :: count = 0
        character(200), public :: name
    end type
    
end module