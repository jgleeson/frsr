
<!-- README.md is generated from README.Rmd. Please edit that file -->

# frsr \[under development\]

<!-- badges: start -->
<!-- badges: end -->

A work-in-progress R package to facilitate the importation of Family
Resources Survey (FRS) data files that you have already downloaded from
the UK Data Service (UKDS). When pointed to a folder where the FRS files
are saved (with their UKDS serial numbers), `frsr` can identify and
import the files for a given year or series of years, join them together
into a single object, standardise variable names and convert labelled
values to factors.

The package does not do anything to help you access the FRS data, which
must be obtained as a registered user of the UKDS. Your use of the data
must be in line with the requirements and conditions of the UKDS that
you agreed to when accessing the data.

## Installation

You can install the development version of `frsr` from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("jgleeson/frsr")
```

Or

``` r
# install.packages("devtools")
devtools::install_github("jgleeson/frsr")
```

## Example

`frsr` currently contains four functions, to import one of the
‘household’, ‘adult’, ‘child’ and ‘benefit unit’ FRS datasets for a
given list of years. For example, if you have already downloaded the FRS
data for 2021 and 2022, and have assigned the path where your datasets
are saved to the ‘folder’ object, the function to import household data
is used as follows.

``` r
library(frsr)

d <- import_hh(folder, c(2021:2022))
```
