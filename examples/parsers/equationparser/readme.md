###### -

![Batteries_Included](./img/Batteries_Included_Icon_300.jpeg)    




# Batteries Included Fortran Library (BiF-lib)

Fortran library source files that assist in file I/O, common operations that are present in other languages, and include data types that mimic python data types (such as, `list` and `dict`).

&nbsp; 

**USGS Software Websites**

- Landing Page: &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; https://www.usgs.gov/software/batteries-included-fortran-library-bif-lib
- Source Repository: &nbsp; &nbsp; https://code.usgs.gov/fortran/bif

&nbsp; 

------

&nbsp; 


## Important Updates

[CHANGELOG.md](CHANGELOG.md) for bug fixes, new features, and updates.



## Direct download

- **[Full Download](https://code.usgs.gov/fortran/bif/-/archive/main/bif-main.zip)**
+ if you have git installed you can obtain the full repository with:  
  `git clone https://code.usgs.gov/fortran/bif.git`



## Code Citation

Boyce, S.E., 2024, Batteries Included Fortran Library (BiF-lib), version 1.2: U.S. Geological Survey Software Release, https://doi.org/10.5066/P9K2IQ6Y



## Additional Citations

Part of these utilities were developed for the following project:

- Boyce, S.E., 2024, MODFLOW One-Water Hydrologic Flow Model (MF-OWHM) Conjunctive Use and Integrated Hydrologic Flow Modeling Software, version 2.x: U.S. Geological Survey Software Release, https://doi.org/10.5066/P9P8I8GS

- Boyce, S.E., Hanson, R.T., Ferguson, I., Schmid, W., Henson, W., Reimann, T., Mehl, S.M., and Earll, M.M., 2020, One-Water Hydrologic Flow Model: A MODFLOW based conjunctive-use simulation software: U.S. Geological Survey Techniques and Methods 6–A60, 435 p., [https://doi.org/10.3133/tm6A60](https://doi.org/10.3133/tm6A60)



## References

When appropriate, the source files include references to the algorithms used in the code. 

This section highlights important publications that are used in this library.




------



The random number generation were based on algorithms described in:

- Blackman, D., and Vigna, S, 2018, Scrambled linear pseudorandom number generators. *arXiv preprint arXiv:1805.01407*.
  - [https://en.wikipedia.org/wiki/Xorshift#xoshiro256**](https://en.wikipedia.org/wiki/Xorshift#xoshiro256**)
  - [http://prng.di.unimi.it/ ](http://prng.di.unimi.it/ )




------



The exponential and normal distribution random number generation were based on algorithms described in:

- Marsaglia G, Tsang WW., 2000, The ziggurat method for generating random variables. Journal of Statistical Software. 2000; 5(8):1–7.
- Doornik, JA., 2005, An improved ziggurat method to generate normal random samples. University of Oxford; 2005.
- McFarland, C. D., 2016, A modified ziggurat algorithm for generating exponentially and normally distributed pseudorandom numbers. Journal of statistical computation and simulation, 86(7), 1281-1294.
  - https://github.com/cd-mcfarland/fast_prng




------



The sorting methods were based on algorithms described in:

- For small arrays, `Insertion Sort`, https://en.wikipedia.org/wiki/Insertion_sort 
- For Stable Sort, `SymMerge`, described in 
  - Kim, P. S., and Kutzner, A., 2004, Stable minimum storage merging by symmetric comparisons. In European Symposium on Algorithms (pp. 714-723). Springer, Berlin, Heidelberg. 
- General Sorting, uses `Introspective Sort` with `Dual Pivot Quicksort` and `Heap Sort`
  - https://en.wikipedia.org/wiki/Introsort
  - Yaroslavskiy, V., 2009, Dual-pivot quicksort. [pdf link](https://codeblab.com/wp-content/uploads/2009/09/DualPivotQuicksort.pdf)
    - https://en.wikipedia.org/wiki/Quicksort#Multi-pivot_quicksort
  - https://en.wikipedia.org/wiki/Heapsort



------



The [Unicode](https://en.wikipedia.org/wiki/Unicode) [UTF-8](https://en.wikipedia.org/wiki/UTF-8) code was written following the specification described in:

 ["Chapter 2. General Structure"](https://www.unicode.org/versions/Unicode6.0.0/). *The Unicode Standard*(6.0 ed.). Mountain View, California, US: [The Unicode Consortium](https://en.wikipedia.org/wiki/The_Unicode_Consortium). ISBN 978-1-936213-01-6.



------




## Let Us Know What  You Are Up To

If you have questions and comments feel free to contact the developers.

Also share any publications that involve the Batteries Included Fortran Library so we can add them to our [publication list](#publications-involving-bif-lib).



## Notes About Repository

### Markdown Files (.md)

[Markdown](https://en.wikipedia.org/wiki/Markdown)   (`.md`) files are placed throughout the repository to assist with informing about different repo aspects or provide general information. Markdown files are simple text files with some additional formatting for fast rendering in html. 

Opening a markdown file in a text editor is human readable, but contains additional markdown syntax. If you search for Markdown viewers you can find a number of tools that will render the final formatting of the `.md` files. 

For more information on markdown viewers, please read the [readme.txt](./readme.txt) file.



# Executive Summary

Standard Fortran provides limited data type, function, and subroutine support leaving the developer to write custom code for every project. This has lead to every developer having their own internal, homemade, Fortran library for doing common operations. In recognition of this, most newer programming languages incorporate a large set of standard data types and functions that the developer can use and reference. For example, the Python language developers called it a "Batteries Included" language because the python standard library includes advanced data types, including sets, lists, and dictionaries (hash tables) and common functions, such as random number generation and fast sorting of data. This Fortran library seeks to emulate many of the advanced features that are a standard part of newer programming languages--hence its name Batteries Included Fortran (BiF). As of now, it is not a complete standard library, but includes many valuable routines for assisting in developing code for numerical models.

The BiF source code is broken into a series of files, each containing a module with a specific theme. That theme is either to provide a generic function interface to a series of routines or data type design (fortran object). An example of an interface is the `NUM2STR` function that converts a Fortran base type into as string and is interfaced to the a set of hidden functions, that includes `INT32_2STR`, `INT32_VEC2STR`, `INT64_2STR`, `INT64_VEC2STR`, `REL64_2STR`, `INT64_VEC2STR`, `TF_2STR_FMT`, and others.  An example of a data type design is the `DATE_OPERATOR` type, which can store time and date information and has a set of functions (methods) attached to the type that allow for common date, time, and calendar operations.

To make the source code, and the modules they contain, easier to understand the majority of the names are appended with::

- `_interface` 
  - To indicates source code contains a generic `INTERFACE` call for a set of subroutines for a specific task.
- `_instruction` 
  - To indicates source code defines one or more `Derived Data Types` definitions (Fortran Objects) and their associated methods (subroutines and functions associated with the object). 




------

[[_TOC_]]     

------


## Source File Description and Compilation Order

The following table provides all the files in the `src` directory. They are grouped by the folders they reside in, which also identify the common theme among the source files.

The dependency **order** is set up such that a file with a higher dependency requires at least one file of a lower dependency order. For example, `date_operator_instruction.f90` has a dependency order of `2`, so it is dependent on at least one file that is of order `1` or `0`. In this case, `date_operator_instruction.f90` is dependent on `calendar_functions.f90`, which has an order number of `1`. 

For an exact listing of each files dependencies, please see open the file [src_dependency_tree.md](src_dependency_tree.md) or [src_dependency_tree.xlsx](src_dependency_tree.xlsx).



| Group                | Name                                             | Description                                                  | Order |
| -------------------- | ------------------------------------------------ | ------------------------------------------------------------ | :---: |
| datetime             | calendar_functions.f90                           | Basic date, time, and calendar functions.                    |   1   |
| datetime             | date_operator_instruction.f90                    | Date and time operations                                     |   2   |
| datetime             | sleep_interface.f90                              | Function that pauses execution for the requested number of seconds. |   1   |
| datetime             | time_series_file_instruction.f90                 | Defines time series type for reading a file that contains a time stamp and associated datum. |   8   |
| datetime             | timer_instruction.f90                            | Basic timing data type                                       |   1   |
| dynamic_arrays       | dynamic_array.f90                                | Wrapper for the dynamic_array_xyz modules                    |   2   |
| dynamic_arrays       | dynamic_array_int32.f90                          | Dynamic allocation array container type for integer(int32) type |   1   |
| error                | error_interface.f90                              | Error and warning routines                                   |   2   |
| error                | warning_type_instruction.f90                     | Data type for reporting warnings and errors                  |   3   |
| input_reader         | buffered_reader_instruction.f90                  | Preloads a file into a buffer for faster reading.  Includes methods for reading from buffer. |   2   |
| input_reader         | generic_block_reader_instruction.f90             | Loads lines of text as a linked list for processing latter. <br />Automatically checks for BEGIN/END clauses for nested loading. |   7   |
| input_reader         | list_array_input_interface.f90                   | List-Array Input (LAI) routines that can automatically load data that is either *STATIC* or *TRANSIENT* and *LIST* or *ARRAY*. |  11   |
| input_reader         | sub_block_input_interface.f90                    | Read multiple block style input that are specified by a transient file reader. |  11   |
| input_reader         | transient_file_reader_instruction.f90            | Instruction on how to process and load *Transient File Reader* input. |  10   |
| input_reader         | uload_and_sfac_interface.f90                     | Universal loading and scale factor routines that can automatically load scalar, vector, and array data. |   9   |
| io                   | cycling_text_file_interface.f90                  | Open WRITE-only, STREAM, UNFORMATTED file that is meant to be overwritten frequently<br />That is, it cycles through one write, then rewinds to the start of the file, then writes a new record. |   6   |
| io                   | file_incrementer_interface.f90                   | Can increment file name with a number to the file name.      |   4   |
| io                   | file_info_interface.f90                          | Queries a file by name or unit number; stores in a data type all Fortran properties about the file. |   1   |
| io                   | file_io_interface.f90                            | File name and unit operations                                |   4   |
| io                   | generic_input_file_instruction.f90               | Open a READ-only file based on keywords.                     |   6   |
| io                   | generic_open_interface.fpp                       | Specifies options to open a file. Requires Fortran Pre-Processing. |   3   |
| io                   | generic_output_file_instruction.f90              | Open a WRITE-only file based on keywords.                    |   6   |
| io                   | post_key_sub.f90                                 | Parse string to get specified post-keywords for file-io <br />Used by GenericInput and GenericOut |   2   |
| io                   | simple_file_io.f90                               | Set of convenience routines for parsing input files and opening read only or write only files.<br/>Uses code already defined in BiF; only use when minimal dependencies are required. |   1   |
| io                   | write_array_interface.f90                        | Write a 1D or 2D array to a file.                            |   2   |
| math_numbers         | descriptive_statistics.f90                       | Provides an routines for calculating basic descriptive statistics (eg mean, variance, etc).<br />Includes a data type for online estimation of the descriptive statistics |   1   |
| math_numbers         | EquationParser.f90                               | Parse string to solve an equation for a given variable set.  |   3   |
| math_numbers         | hexadecimal_instruction.f90                      | Provides hexadecimal structure with basic operations, such as addition   and conversion to Unicode point. |   1   |
| math_numbers         | isqrt_interface.f90                              | Floor square root of integer using bisection method.         |   1   |
| math_numbers         | log2_interface.f90                               | Log2 functions                                               |   1   |
| math_numbers         | number_conversion_interface.f90                  | Convert numbers between decimal, binary, and hexadecimal bases and hexadecimal bytes.<br />Binary and hexadecimal numbers are stored as strings. |   1   |
| math_numbers         | power_interface.f90                              | Provides optimized routines for solving 10^x <br />and a function that returns the closest power of 2 to provided number |   1   |
| math_numbers         | prime_finder.f90                                 | Functions that return the next largest prime number <br />or check if a number is prime. |   1   |
| math_numbers         | random_routines_interface.f90                    | Random number generation routines for uniform, exponential, and normal distribution.<br />Can also shift values within a vector to randomize the order. |   1   |
| math_numbers         | relax_interface.f90                              | Relax and Dampening routines                                 |   1   |
| math_numbers         | secant_interface.f90                             | Given a user supplied function solves for the root via the secant and bisection methods. |   1   |
| sort                 | sort_interface_ascii.f90                         | Sorting 1D and 2D character data in ASCII, Case-less, or natural order. |   2   |
| sort                 | sort_interface_driver.f90                        | Main driver module for sorting 1D and 2D data using one of four different sort methods. |   2   |
| sort                 | sort_interface_int32.f90                         | Sorting 1D and 2D integer(Int32) data                        |   2   |
| sort                 | sort_interface_int64.f90                         | Sorting 1D and 2D integer(Int64) data                        |   2   |
| sort                 | sort_interface_multi.f90                         | Sorting multiple 1D arrays together as if they are a 2D array.<br />Each 1D array can be a different base type (such as Integer and Real). |   2   |
| sort                 | sort_interface_rel32.f90                         | Sorting 1D and 2D real(real32) data                          |   2   |
| sort                 | sort_interface_rel64.f90                         | Sorting 1D and 2D real(real64) data                          |   2   |
| sort                 | sort_interface_wild.f90                          | Generic sort interface that can sort any kind of 1D data by using a user-defined LESS and SWAP functions.<br />This allows sorting of custom data types or data groups. |   2   |
| spatial              | adjacency_list_instruction_and_shortest_path.f90 | Adjacency List Type that supports shortest path searches via the Dijkstra algorithm. |   4   |
| spatial              | obs_group_interpolator.f90                       | Observation point interpolation algorithms for cartesian space. |   3   |
| spatial              | xy_grid_coordinate_interface.f90                 | Rectangular cartesian coordinate system for structured grids. |   1   |
| strings              | cast_to_string_interface.f90                     | Interface that converts the byte representation of any variable to a string.<br />This is equivalent to reading a record written to a STREAM UNFORMATTED file with a CHARACTER type. |   1   |
| strings              | is_ascii_interface.f90                           | Check if string contains only ASCII characters.              |   3   |
| strings              | line_writer_interface.f90                        | Write to a buffered line text at specified positions.        |   2   |
| strings              | num2str_interface.f90                            | Convert any **INT**, **SNG**, **DBL**, and **Logical** to a pretty formatted string |   1   |
| strings              | parse_word_interface.f90                         | Parse string to next word separated by space, tab, and comma |   1   |
| strings              | string_routines.f90                              | String manipulation functions and GET_ routines.<br />GET_xyz routines covert string ABC to another string or data type xyz. |   5   |
| system               | console_commander.f90                            | Provides access to writing to command prompt with backspace for updating |   1   |
| system               | directory_iso_c_binding.f90                      | Provides functions to change current directory, make a directory, and get the current path.<br />Can also determine if OS is Windows_NT or not. |   1   |
| system               | path_interface.f90                               | Creates missing directories in a path. Performs slash and back-slash operations |   2   |
| system               | system_call.f90                                  | Execute a command using the operating system shell and return the output from the command. |   1   |
| types_and_containers | array_data_types_instruction.f90                 | Data type that provide easy array access                     |   1   |
| types_and_containers | binary_heap_instruction.f90                      | Binary heap data type and sorting algorithm                  |   1   |
| types_and_containers | circular_queue_instruction.f90                   | Circular queue, which is a linked list that has the end of the list point to the start. |   1   |
| types_and_containers | hash_table_instruction.f90                       | Hash table data type that mimics a python dictionary to store all the Fortran base types. |   2   |
| types_and_containers | integer_array_builder.f90                        | Simple auto-allocation integer array for appending.          |   1   |
| types_and_containers | integer_queue_instruction.f90                    | Simple push/pull linked list que                             |   1   |
| types_and_containers | IXJ_instruction.f90                              | Data structure that reads and stores a list of data structured as a set of integers (`I`), then numbers (`X`), and then integers (`J`). |   8   |
| types_and_containers | linked_list_instruction.f90                      | Linked List Data Type base, **INT**, and **CHAR**            |   1   |
| types_and_containers | lookup_table_instruction.f90                     | Defines lookup table data type for reading sets of lookup tables and querying them. |   8   |
| types_and_containers | name_id_interface.f90                            | Data type for associating a name with a model grid coordinate and value |   1   |
| types_and_containers | rolling_pointer_instruction.f90                  | Pointer array for *int32* or *int64* that is useful for keeping track of past values or fast shifting of values by pointer re-assignment. |   1   |
| types_and_containers | variable_pointer_list_interface.f90              | Contains array of pointers designed for collecting a different variable targets into one type. |   1   |
| unicode              | unicode_interface.f90                            | Functions for parsing unicode strings or returning a unicode character for a given code point. |   1   |
| unit_test            | unit_testing_instruction.f90                     | Data type that provides unit test methods, such as assert.<br />Provides bookkeeping of what tests fail and pass. |   1   |
| util_misc            | alloc_interface.f90                              | Allocate arrays with lots of options                         |   1   |
| util_misc            | constants.f90                                    | Defines a set of parameter variables that are used by the rest of the library. |   0   |
| util_misc            | is_routine_interface.f90                         | IS_ query functions. <br />For example, `IS_ODD` returns if the number is odd. |   1   |
| util_misc            | position_interface.f90                           | Return index where a value is in a vector                    |   2   |
| util_misc            | same_memory_address_interface.f90                | Routine that tests if two variables occupy the same ram-memory location. |   1   |
| util_misc            | set_array_interface.f90                          | Fast setting arrays to a value, zero, or NaN                 |   1   |
| util_misc            | util_interface.f90                               | Simple utility routines for processing numbers<br />(eg, *near_zero* and *vec_adjust_maxsum*) that were not large enough to standalone in a file. |   2   |



------



# Publications Involving BiF-lib

## Basic Documentation and Code Publications

Boyce, S.E., Hanson, R.T., Ferguson, I., Schmid, W., Henson, W., Reimann, T., Mehl, S.M., and Earll, M.M., 2020, One-Water Hydrologic Flow Model: A MODFLOW based conjunctive-use simulation software: U.S. Geological Survey Techniques and Methods 6–A60, 435 p., [https://doi.org/10.3133/tm6A60](https://doi.org/10.3133/tm6A60)



## Application Bibliography

Alattar, M., Troy, T., Russo, T. and Boyce, S. E., 2020, Modeling the surface water and groundwater budgets of the US using MODFLOW-OWHM. Advances in Water Resources, 143, p. 103682, https://doi.org/10.1016/j.advwatres.2020.103682 

Boyce, S.E., and Yeh, W.G., 2014, Parameter-independent model reduction of transient groundwater flow models: Application to inverse problems, Advances in Water Resources, 69, pp. 168–180, http://dx.doi.org/10.1016/j.advwatres.2014.04.009

Boyce, S.E., Nishikawa, T., and Yeh, W.G., 2015, Reduced order modeling of the Newton formulation of MODFLOW to solve unconfined groundwater flow: Advances in Water Resources, 83, pp. 250-262. http://dx.doi.org/10.1016/j.advwatres.2015.06.005

Boyce, S.E., 2015, Model Reduction via Proper Orthogonal Decomposition of Transient Confined and Unconfined Groundwater-Flow: PhD Dissertation, Dept. of Civil Engineering, University of California at Los Angeles, 64p.

Hanson, R.T., Traum J., Boyce, S.E., Schmid, W., Hughes, J.D, W. W. G., 2015, Examples of Deformation-Dependent Flow Simulations of Conjunctive Use with MF-OWHM. Ninth International Symposium on Land Subsidence (NISOLS), Nagoya, Japan, 6p.

Hanson, R.T., Ritchie, A.B., Boyce, S.E., Galanter, A.E., Ferguson, I.A., Flint, L.E., and Henson, W.R., 2020, Rio Grande transboundary integrated hydrologic model and water-availability analysis, New Mexico and Texas, United States, and Northern Chihuahua, Mexico: U.S Geological Survey Scientific Investigations Report 2020–xxxx, xxx p.

Henson, W., Hanson, R.T., Boyce, S.E., 2020 (in press), Integrated Hydrologic model of the Salinas Valley, Monterey County, California: U.S Geological Survey Scientific Investigations Report 2020–xxxx, xxx p.

Rossetto, R., De Filippis, G., Triana, F., Ghetta, M., Borsi, I., Schmid, Wolfgang, 2019, Software tools for management of conjunctive use of surface- and groundwater in the rural environment: integration of the Farm Process and the Crop Growth Module in the FREEWAT platform: Agricultural Water Management, Vol 223, No. 105717, 18p. (https://doi.org/10.1016/j.agwat.2019.105717)

Russo, T.A, 2012, Hydrologic System Response to Environmental Change: Three Case Studies in California, PhD Dissertation, Department of Earth Sciences, University of California at Santa Cruz, 56p.

Russo, T.A, Fisher, A.T., and Lockwood, B.S., 2014, Assessment of Managed Aquifer Recharge Site Suitability Using a GIS and Modeling, Ground Water, pp.1-12, doi: 10.1111/gwat.12213



------



# Disclaimer
Although the software has been subjected to rigorous review, it represents a specific compilation for the source code presented in https://code.usgs.gov/fortran/bif using the commit with the same version number (either release/stable or beta/experimental version). 

Binary versions may not contain bug fixes that are applied to high version number. Generally the highest available version number will contain the most up to date set of bug fixes, features, and had the most historical code review. Lower version binaries are kept on this repository to have a historical record and easy access to compiled binaries used in the past.

The USGS reserves the right to update the software as needed pursuant to further analysis and review. No warranty, expressed or implied, is made by the USGS or the U.S. Government as to the functionality of the software and related material nor shall the fact of release constitute any such warranty. Furthermore, the software is released on condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from its authorized or unauthorized use.