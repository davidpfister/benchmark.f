<!--! @mainpage -->
<h1 class="title">
    <span class="name">Benchmark.f</span>
    <br>
    <span class="text">Benchmarking with precision</span>
    <br>
    <span class="tagline">
    A KISS library for benchmarking Fortran functions and subroutines.
    </span>
</h1>
<br>

<div class="actions">
    <div class="action">
        <a class="button medium brand" href="index.html#autotoc_md2">Get Started</a>
    </div>
    <div class="action">
        <a class="button medium alt" href="topics.html">API</a>
    </div>
    <div class="action">
        <a class="button medium alt" href="https://github.com/davidpfister/benchmark.f" target="_blank" rel="noreferrer">View on GitHub</a>
    </div>
</div>

# Introduction
<!-- ABOUT THE PROJECT -->
## About the Project

<div style="text-align: center;">
  <img src="https://github.com/davidpfister/benchmark.f/blob/master/.dox/images/screenshot.png?raw=true" width="512" height="512">
</div>


Fortran is the fastest language on earth, so they say. But can we prove it? <br>
Despite its legendary speed when crunching numbers, Fortran is no exception when it comes to writing code: it's also very possible to write terribly slow pieces of code. This is where benchmarking different implementations of the same function can help developing better and faster algorithms.  

This project aims at providing an easy interface to benchmark functions and subroutines while taking care of warming up the machine, collecting system information, computing statistics and reporting results. 

* [![fpm][fpm]][fpm-url]
* [![ifort][ifort]][ifort-url]
* [![gfortran][gfortran]][gfortran-url]

<!-- GETTING STARTED -->
## Installation

### Requirements

To build that library you need

- a Fortran 2008 compliant compiler, or better, a Fortran 2018 compliant compiler (Intel Fortran compiler is known to work well for _benchmark.f_. gfortran has some limitations when using implicit procedures and unlimited polymorphic arguments. Please refer to the [documentation](https://davidpfister.github.io/benchmark.f/compiler_differences.html) to see the difference between compilers).

The following compilers are tested on the default branch of _benchmark.f_:

<center>

| Name |	Version	| Platform	| Architecture |
|:--:|:--:|:--:|:--:|
| GCC Fortran (MinGW) | 14 | Windows 10 | x86_64 |
| Intel oneAPI classic	| 2021.5	| Windows 10 |	x86_64 |

</center>

- a preprocessor. _benchmark.f_ uses quite some preprocessor macros. It is known to work both with intel `fpp` and `cpp`.  

Unit test rely on the the header file [`assertion.inc`](https://github.com/davidpfister/fortiche/tree/master/src/assertion). Since the whole framework fits in a single file, it has been added directly to the repo. 

Linting, indentation, and styling is done with [codee](https://www.codee.com/codee-formatter/) and [fortitude](https://fortitude.readthedocs.io/en/stable/) with the following settings
```bash
codee format ./src
fortitude check ./src --fix
```

#### Get the code
```bash
git clone https://github.com/davidpfister/benchmark.f
cd benchmark.f
```

#### Build with fpm

The repo can be build using _fpm_
```bash
fpm build
```
For convenience, the  repo also contains a response file that can be invoked as follows: 
```
fpm @build
```
(For the Windows users, that command does not work in Powershell since '@' is a reserved symbol. One should use the '--%' as follows: `fpm --% @build`.
This is linked to the following [issue](https://github.com/urbanjost/M_CLI2/issues/19))

Building with ifort requires to specify the compiler name (gfortran by default)
```bash
fpm @build --compiler ifort
```
Alternatively, the compiler can be set using fpm environment variables.
```bash
set FPM_FC=ifort
```

Besides the build command, several commands are also available:
```bash
@pretiffy
system codee format ./src
system fortitude check ./src --fix
option run --list

@clean
option clean --all

@rebuild
system rmdir /s /q build
option build

@build
option build

@test
options test

@doc
system cd ./.dox & doxygen ./Doxyfile.in & cd ..
system powershell ./tools/Fix-Doxygen.ps1 -Path "./docs"
option run --list
```

The toml files contains two items that are worth commenting.

**The settings to the cpp preprocessor are specified in the file.**

```ini
[preprocess]
cpp.suffixes = ["F90", "f90"]
cpp.macros = ["_FPM"]
```
The `_FPM` macro is used to differentiate the build when compiling with _fpm_ or _Visual Studio_. This is mostly present to adapt the hard coded paths that differs in both cases.

**The code must also be compiled allowing implicit procedures.**
This is reflected in the following option. 

```ini
[fortran]
implicit-external = true
```
In order to be able to benchmark functions AND subroutines with any number of dummy arguments (0 to 7 at the moment) of any types (intrinsic or derived types), implicit procedures are a must. While this may be considered as bad practice and a remainder from F77 and the good old external, there would be no other way to provide a generic library without this option. 

#### Build with Visual Studio 2019

The project was originally developed on Windows with Visual Studio 2019. The repo contains the solution file (_Benchmark.f.sln_) to get you started with Visual Studio 2019. 

<!-- LICENSE -->
<!-- USAGE EXAMPLES -->
## Quick Start

Running the benchmark could not be simpler. 

1. Start by including the file `benchmark.inc` into your code
2. Instantiate a benchmark runner 
3. Run the benchmark

The first step is to create a test function. It can be a function or a subroutine (gfortran only handles subroutine. For more issues related to gfortran, see [this article](https://davidpfister.github.io/benchmark.f/compiler_differences.html) ) with any number of arguments between 0 and 7. 
```fortran
!the funcion to be benchmarked
subroutine test_function()
...
end subroutine
```
And then simply call the `benchmark` macro.
```fortran
#include <benchmark.inc>
program test
use benchmark_library

type(runner) :: br

benchmark(br, run(test_function))
```
This will generate this kind of table: 

     |         Method Name      |          Mean          |    Standard Deviation  |
     |__________________________|________________________|________________________|
     |test_function()           |           217350.000 us|          +/- 161306.626|

_For more examples, please refer to the [Documentation](https://davidpfister.github.io/benchmark.f/examples_toc.html)_

The library takes care of everything else for you
- Collection of system information
- Collection of compiler information
- Collection of compilation options
- Reporting

## Contributing

Contributions are what make the open source community such an amazing place to learn, inspire, and create. Any contributions you make are **greatly appreciated**. So, thank you for considering contributing to _benchmark.f_.
Please review and follow these [guidelines](https://github.com/davidpfister/benchmark.f/tree/master?tab=contributing-ov-file) to make the contribution process simple and effective for all involved. In return, the developers will help address your problem, evaluate changes, and guide you through your pull requests.

By contributing to _benchmark.f_, you certify that you own or are allowed to share the content of your contribution under the same license.
## License

Distributed under the MIT License.

[![Contributors][contributors-shield]][contributors-url]
[![Forks][forks-shield]][forks-url]
[![Stargazers][stars-shield]][stars-url]
[![Issues][issues-shield]][issues-url]
[![MIT License][license-shield]][license-url]

<!-- MARKDOWN LINKS & IMAGES -->
[contributors-shield]: https://img.shields.io/github/contributors/davidpfister/benchmark.f.svg?style=for-the-badge
[contributors-url]: https://github.com/davidpfister/benchmark.f/graphs/contributors
[forks-shield]: https://img.shields.io/github/forks/davidpfister/benchmark.f.svg?style=for-the-badge
[forks-url]: https://github.com/davidpfister/benchmark.f/network/members
[stars-shield]: https://img.shields.io/github/stars/davidpfister/benchmark.f.svg?style=for-the-badge
[stars-url]: https://github.com/davidpfister/benchmark.f/stargazers
[issues-shield]: https://img.shields.io/github/issues/davidpfister/benchmark.f.svg?style=for-the-badge
[issues-url]: https://github.com/davidpfister/benchmark.f/issues
[license-shield]: https://img.shields.io/github/license/davidpfister/benchmark.f.svg?style=for-the-badge
[license-url]: https://github.com/davidpfister/benchmark.f/master/LICENSE
[gfortran]: https://img.shields.io/badge/gfortran-000000?style=for-the-badge&logo=gnu&logoColor=white
[gfortran-url]: https://gcc.gnu.org/wiki/GFortran
[ifort]: https://img.shields.io/badge/ifort-000000?style=for-the-badge&logo=Intel&logoColor=61DAFB
[ifort-url]: https://www.intel.com/content/www/us/en/developer/tools/oneapi/fortran-compiler.html
[fpm]: https://img.shields.io/badge/fpm-000000?style=for-the-badge&logo=Fortran&logoColor=734F96
[fpm-url]: https://fpm.fortran-lang.org/
