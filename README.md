
# datachemist

<!-- badges: start -->
<!-- badges: end -->

The goal of datachemist is to provide an example for how chemists can utilize R in their lab groups for comprehensive reproducible data analysis, similar to implementing a lab SOP.

## Installation

You can install the released version of datachemist from github with:

``` r
devtools::install_github("vestoj/datachemist")
```

## Example

This is a basic example which shows you how to use the calibrate function to perform a weighted linear regression (outputs a neat plot, equation, and prints the equation):

``` r
library(datachemist)
calibrate(ICPMS$signal, ICPMS$RSD, ICPMS$conc, "Pb", "um", "AU")
```

