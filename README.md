
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tardis

<!-- badges: start -->
<!-- badges: end -->

*Functions for processing AG501 EMA data and converting to
EMU-compatible formats*

The tardis package contains functions for two primary purposes (1)
processing position-calculated and head-corrected EMA sensor data from
the Carstens AG501 system, including calculating derivatives and further
derived variables; (2) converting processed files to the SSFF format for
use with the EMU system, as well as calculating MFCCs from WAV files and
converting these to SSFF format. These functions can be used
independently, although conversion to SSFF makes strong assumptions
about the structure of your AG501 .txt files.

This package is in-progress and currently undergoing testing. Use at
your own caution!

## Installation

You can install tardis as follows:

``` r
remotes::install_github("samkirkham/tardis", build_vignettes = TRUE)
```

## Example

To see an example of typical workflow and function usage:

``` r
vignette("tardis-workflow", package = "tardis")
```
