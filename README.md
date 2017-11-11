
<!-- README.md is generated from README.Rmd. Please edit that file -->
<img src="man/figures/logo.png" alt="CoRC logo" width="200"> --- the <b>Co</b>pasi <b>R</b> <b>C</b>onnector
============================================================================================================

Overview
--------

Development Stage: **Internal Beta**

CoRC, the Copasi R Connector, links the Complex Pathway Simulator COPASI ([copasi.org](http://copasi.org)) and the (statistical) programming environment R ([r-project.org](http://r-project.org)). It provides easy access to the powerful biochemical model editing, simulation and analysis backend of Copasi from the convenient R command line interface. This allows the user to develop elaborate scripts and workflows for analyses that would require a great deal of tedious manual work otherwise. These scripts can then be run interactively from the R command line interface or be sent to cluster or cloud facilities for more demanding calculations.

CoRC features:

-   high-level API for Copasi in the R language
-   define reproducible workflows using Copasi's powerful modelling tools
-   easy parallization of Copasi tasks
-   enables straight forward publication-ready plotting with the R ecosystem
-   ...

It is based on a current development version of Copasi but is expected to closely follow official Copasi releases in the future.

CoRC comes with the Artistic License 2.0. By using CoRC you agree to this license.

Installation
------------

Install the development version from GitHub.

``` r
# install.packages("devtools")
devtools::install_github("jpahle/CoRC")
CoRC::getCopasi()
```

Usage
-----

``` r
library(CoRC)
loadExamples(1)
#> [[1]]
#> # A copasi model reference:
#> Model name: "The Brusselator"
#> Number of compartments: 1
#> Number of species: 6
#> Number of reactions: 4

runTimeCourse()$result
#> # A tibble: 201 x 3
#>     Time         X         Y
#>    <dbl>     <dbl>     <dbl>
#>  1   0.0 3.0000000 3.0000000
#>  2   0.5 3.4081600 0.8174841
#>  3   1.0 1.8964569 1.2767901
#>  4   1.5 0.8762539 1.8729300
#>  5   2.0 0.3459334 2.3681908
#>  6   2.5 0.1827622 2.6585981
#>  7   3.0 0.1474333 2.8636840
#>  8   3.5 0.1411289 3.0483214
#>  9   4.0 0.1407334 3.2283474
#> 10   4.5 0.1415567 3.4069724
#> # ... with 191 more rows
```
