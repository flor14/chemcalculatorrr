
<!-- README.md is generated from README.Rmd. Please edit that file -->

# chemcalculatorrr

<!-- badges: start -->

[![codecov](https://codecov.io/gh/flor14/chemcalculatorrr/branch/main/graph/badge.svg?token=LCYTGH2IYU)](https://codecov.io/gh/flor14/chemcalculatorrr)

[![codecov](https://codecov.io/gh/UBC-MDS/chemcalculatorrr/branch/master/graph/badge.svg?token=nVHX32wNQP)](https://codecov.io/gh/UBC-MDS/chemcalculatorrr)
<!-- badges: end -->

The goal of chemcalculatorrr is R package useful for chemistry for the
purpose of calculating chemical formular mass in g/mol. The pacakage is
designed to perform the same function as the [python
package](https://github.com/UBC-MDS/chemcalculator). The mole allows
scientists to calculate the number of elementary entities (usually atoms
or molecules) in a certain mass of a given substance. Also, the mass of one mole of a substance is
equal to that substance’s molecular weight; as for instance, the mean
molecular weight of water is 18.015 atomic mass units (amu), so one mole
of water weighs 18.015 grams. This property simplifies many chemical
computations. This R package will be helpful to easily calculate the
chemical formula mass, convert moles to grams and vice versa, and lastly
calculate the percentage mass for the atomic nature of the elements in
chemistry.

## Features

This package contains three functions. Each function will have it’s own
required and optional arguments.

1.  `compute_mass`: Calculate the mass of the atoms or chemical formula
    for the input chemical formula.
2.  `moles_grams_converter`: Convert moles to grams and convert grams to
    moles.
3.  `percent_mass`: Calculate percentage mass for the desired atom or
    molecule.

## Installation

You can install the released version of chemcalculatorrr from with:

``` r
devtools::install_github("UBC-MDS/chemcalculatorrr")
```

## Relevance to the R Ecosystem

To our knowledge, there is no general-purpose library for calculating
chemical formula mass in g/mol in the R ecosystem. We believe that
`chemcalculatorrr` will provide some useful functionality to know how to
calculate moles, and provide a grams to moles calculator, or even a
moles to grams calculator. With our moles to grams converter, you can
seamlessly convert between mass, molecular weight and moles.

## Usage

`chemcalculatorrr` can be used as follows:

``` r
library(chemcalculatorrr)
compute_mass("H2O")
```

``` r
library(chemcalculatorrr)
moles_grams_converter("H2O", 0.05555, "moles")
```

``` r
library(chemcalculatorrr)
percent_mass("H2O", "O")
```

## Documentation

The help file can be viewed by:

``` r
?chemcalculatorrr::compute_mass
?chemcalculatorrr::moles_grams_converter
?chemcalculatorrr::percent_mass
```

## Contributors

### Development Lead

| Contributor Name | GitHub Username                                 |
|------------------|-------------------------------------------------|
| Kingslin Lv      | [Kingslin0810](https://github.com/Kingslin0810) |
| Joyce Wang       | [jo4356](https://github.com/jo4356)             |
| Allyson Stoll    | [datallurgy](https://github.com/datallurgy)     |

We welcome and recognize all contributions. Please find the guide for
contribution in [Contributing
Document](https://github.com/UBC-MDS/chemcalculatorrr/blob/main/.github/CONTRIBUTING.md).
