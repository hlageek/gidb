
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{gidb}`

<!-- badges: start -->

<!-- badges: end -->

## Installation

You can install the development version of `{gidb}` like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Run

You can launch the application by running:

``` r
gidb::run_app()
```

## About

You are reading the doc about version : 0.0.0.9000

This README has been compiled on the

``` r
Sys.time()
#> [1] "2026-02-08 21:15:46 CET"
```

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> ══ Documenting ═════════════════════════════════════════════════════════════════
#> ℹ Installed roxygen2 version (7.3.3) doesn't match required (7.1.1)
#> ✖ `check()` will not re-document this package
#> ── R CMD check results ──────────────────────────────────── gidb 0.0.0.9000 ────
#> Duration: 16.7s
#> 
#> ❯ checking DESCRIPTION meta-information ... WARNING
#>   Non-standard license specification:
#>     What license is it under?
#>   Standardizable: FALSE
#> 
#> 0 errors ✔ | 1 warning ✖ | 0 notes ✔
#> Error: R CMD check found WARNINGs
```

``` r
covr::package_coverage()
#> Error in loadNamespace(x): there is no package called 'covr'
```
