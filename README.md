<a id="readme-top"></a>

[![Contributors][contributors-shield]][contributors-url]
[![Forks][forks-shield]][forks-url]
[![Stargazers][stars-shield]][stars-url]
[![Issues][issues-shield]][issues-url]
[![MIT License][license-shield]][license-url]

<!-- PROJECT LOGO -->
<br />
<div align="center">
  <h3 align="center">Benchmark.f</h3>

  <p align="center">
    A KISS library for benchmarking Fortran functions and subroutines with precision
    <br />
    <a href="https://github.com/davidpfister/benchmark.f"><strong>Explore the project »</strong></a>
    <br />
  </p>
</div>



<!-- TABLE OF CONTENTS -->
[TOC]

# Introduction
<!-- ABOUT THE PROJECT -->
## About the Project
<center>

![Benchmark Screen Shot](https://github.com/davidpfister/benchmark.f/blob/master/.dox/images/screenshot.png?raw=true)

</center>
Fortran is the fastest language on earth, so they say. But can we prove it? <br><br>
And despite its legendary speed when it comes to crunching numbers, Fortran is no exception when it comes to writing code: it's also very possible to write terribly slow pieces of code. This is where benchmarking different implementations of the same function can help developing better and faster algorithms.  

This project aims at providing an easy interface to benchmark functions and subroutines while taking care of warming up the machine, collecting system information, computing statistics and reporting results. 

* [![fpm][fpm]][fpm-url]
* [![ifort][ifort]][ifort-url]
* [![gfortran][gfortran]][gfortran-url]

<!-- GETTING STARTED -->
## Getting Started

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
- a preprocessor. _benchmark.f_ uses quite some preprocessor macros. It is known to work both with intel `fpp` an gnu `cpp`.  

Unit test rely on the the header file `assert.inc`. Since the whole framework fits in a single file, it has been added directly to the repo. 

Linting, indentation, and styling is done with [fprettify](https://github.com/fortran-lang/fprettify) with the following settings
```bash
fprettify .\src\ -r --case 1 1 1 1 -i 4 --strict-indent --enable-replacements --strip-comments --c-relations
```

### Installation

#### Get the code
```bash
git clone https://github.com/davidpfister/benchmark.f
cd benchmark.f
```

#### Build with fpm

The repo is compatible with fpm projects. It can be build using _fpm_
```bash
fpm build --flag '-ffree-line-length-none'
```
For convenience, the  repo also contains a response file that can be invoked as follows: 
```
fpm @build
```
(For the Windows users, that command does not work in Powershell since '@' is a reserved symbol. One should
use the '--%' as follows: `fpm --% @build`.
This is linked to the following [issue](https://github.com/urbanjost/M_CLI2/issues/19))

Building with ifort requires to specify the compiler name (gfortran by default)
```cmd
fpm @build --compiler ifort
```
Alternatively, the compiler can be set using fpm environment variables.
```cmd
set FPM_FC=ifort
```

Besides the build command, several commands are also available:
```cmd
@pretiffy
system fprettify .\examples\ -r --case 1 1 1 1 -i 4 --strict-indent --enable-replacements --strip-comments --c-relations
system fprettify .\src\ -r --case 1 1 1 1 -i 4 --strict-indent --enable-replacements --strip-comments --c-relations
system fprettify .\tests\ -r --case 1 1 1 1 -i 4 --strict-indent --enable-replacements --strip-comments --c-relations

@clean
option clean --all

@rebuild
system rmdir /s /q build
option build --flag '-ffree-line-length-none'

@build
option build --flag '-ffree-line-length-none'

@test
options test --flag '-ffree-line-length-none'

@doc
option clean --all
system cd ./.dox & doxygen ./Doxyfile.in & cd ..
```

The toml files contains two items that are worth commenting: 
1. The settings to the cpp preprocessor are specified in the file. 

```toml
[preprocess]
cpp.suffixes = ["F90", "f90"]
cpp.macros = ["_FPM"]
```
The `_FPM` macro is used to differenciate the build when compiling with _fpm_ or _Visual Studio_. This is mostly present to adapt the hard coded paths that differs in both cases.

2. The code must also be compiled allowing implicit procedures. This is reflected in the following option. 
```
[fortran]
implicit-external = true
```
In order to be able to benchmark functions AND subroutines with any number of dummy arguments (0 to 7 at the moment) of any types (intrinsic or derived types), implicit procedures are a must. While this may be considered as bad practice and a remainder from F77 and the good old external, there would be no other way to provide a generic library without this option. 

#### Build with Visual Studio 2019

The project was originally developed on Windows with Visual Studio 2019. The repo contains the solution file (_Benchmark.f.sln_) to get you started with Visual Studio 2019. 


<!-- USAGE EXAMPLES -->
## Usage

Running the benchmark could not be simpler. 

1. Start by including the file `benchmark.inc` into your code
2. Instantiate a benchmark runner 
3. Run the benchmark

```fortran
#include <benchmark.inc>
program test
use benchmark_library

type(runner) :: br

benchmark(br, run(1.0d-6, 30, test_function))
```
and generates this kind of table: 

     |         Method Name      |          Mean          |    Standard Deviation  |
     |__________________________|________________________|________________________|
     |test_poisson(1.0d-6,30)   |           217350.000 us|          +/- 161306.626|
     |test_poisson(1.0d-6,30)   |            99250.000 us|            +/- 7588.643|
     |test_poisson(.10E-05,30)  |           176550.000 us|          +/- 135795.609|

_For more examples, please refer to the [Documentation](https://davidpfister.github.io/benchmark.f.github.io/examples_toc.html)_

The library takes care of everything else for you
- Collection of system information
- Collection of compiler information
- Collection of compilation options
- Reporting

<!-- CONTRIBUTING -->
### Contributing

Contributions are what make the open source community such an amazing place to learn, inspire, and create. Any contributions you make are **greatly appreciated**. So, thank you for considering contributing to _benchmark.f_.
Please review and follow these guidelines to make the contribution process simple and effective for all involved. In return, the developers will help address your problem, evaluate changes, and guide you through your pull requests.

By contributing to _benchmark.f_, you certify that you own or are allowed to share the content of your contribution under the same license.

### Style

Please follow the style used in this repository for any Fortran code that you contribute. This allows focusing on substance rather than style.

### Reporting a bug

A bug is a *demonstrable problem* caused by the code in this repository.
Good bug reports are extremely valuable to us—thank you!

Before opening a bug report:

1. Check if the issue has already been reported
   ([issues](https://github.com/davidpfister/benchmark.f/issues)).
2. Check if it is still an issue or it has been fixed?
   Try to reproduce it with the latest version from the default branch.
3. Isolate the problem and create a minimal test case.

A good bug report should include all information needed to reproduce the bug.
Please be as detailed as possible:

1. Which version of _benchmark.f_ are you using? Please be specific.
2. What are the steps to reproduce the issue?
3. What is the expected outcome?
4. What happens instead?

This information will help the developers diagnose the issue quickly and with
minimal back-and-forth.

### Pull request

If you have a suggestion that would make this project better, please create a pull request. You can also simply open an issue with the tag "enhancement".
Don't forget to give the project a star! Thanks again!
1. Open a [new issue](https://github.com/davidpfister/benchmark.f/issues/new) to
   describe a bug or propose a new feature.
   Refer to the earlier sections on how to write a good bug report or feature    request.
2. Discuss with the developers and reach consensus about what should be done about the bug or feature request.
   **When actively working on code towards a PR, please assign yourself to the
   issue on GitHub.**
   This is good collaborative practice to avoid duplicated effort and also inform others what you are currently working on.
3. Create your Feature Branch (```git checkout -b feature/AmazingFeature```)
4. Commit your Changes (```git commit -m 'Add some AmazingFeature'```)
5. Push to the Branch (```git push origin feature/AmazingFeature```)
6. Open a Pull Request with your contribution.
   The body of the PR should at least include a bullet-point summary of the
   changes, and a detailed description is encouraged.
   If the PR completely addresses the issue you opened in step 1, include in
   the PR description the following line: ```Fixes #<issue-number>```. If your PR implements a feature that adds or changes the behavior of _benchmark.f_,
   your PR must also include appropriate changes to the documentation and associated units tests.

In brief, 
* A PR should implement *only one* feature or bug fix.
* Do not commit changes to files that are irrelevant to your feature or bug fix.
* Smaller PRs are better than large PRs, and will lead to a shorter review and
  merge cycle
* Add tests for your feature or bug fix to be sure that it stays functional and useful
* Be open to constructive criticism and requests for improving your code.


<!-- LICENSE -->
## License

Distributed under the MIT License.

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
