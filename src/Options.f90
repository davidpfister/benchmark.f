module benchmark_options
    use benchmark_kinds
    
    implicit none
    
    type, public :: runner_options
        integer, public     :: repetitions = 10 !< number of repetitions
        integer, public     :: maxcalls = 10000000 !< maximum number of function calls
        real(r8), public    :: mintime = 100.0_r8 !< minimum sampling time in ms to collect data
        real(r8), public    :: maxtime = 100000.0_r8 !< maximum sampling time in ms to collect data
        real(r8), public    :: offset = 0.0_r8 !< maximum sampling time in ms to collect data
        logical, public     :: skip_warmup = .false. !< option to skip warm up phase
        logical, public     :: skip_dryrun = .false. !< option to skip dry run phase
        integer, public     :: sampling_window = 10 !< option to adjust the size of the sampling window
        integer, public     :: ssd_threshold = 0.05_r8 !< acceptance threshold for the steady-state detection
        !private
        integer, public :: count = 0
        character(200), public :: name
    end type
    
end module