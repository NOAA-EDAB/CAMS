Initial Comparisons
================
Sean Lucey
1/18/2022

These are some initial comparisons between the 2019 AA and CAMS tables.
Data were pulled using the `get_comland_raw_data` function from the R
package `comlandr` created and maintained by the Ecosystem Dynamics and
Assessment Branch (EDAB) of the Northeast Fisheries Science Center
(NEFSC).

``` r
load(file = here('data-raw', 'CAMS_2019.RData'))
load(file = here('data-raw', 'AA_2019.RData'))
#move data out to main object
aa <- aa$comland
```

First some simple metrics

``` r
nrow(cams)
```

    ## [1] 52982

``` r
nrow(aa)
```

    ## [1] 57664

Total landings

``` r
cams[, sum(SPPLIVMT)]
```

    ## [1] 572608.6

``` r
aa[, sum(SPPLIVMT)]
```

    ## [1] 575854.3

Landings that are missing area

``` r
cams[AREA == 0, sum(SPPLIVMT)]
```

    ## [1] 57709.21

``` r
aa[AREA == 0, sum(SPPLIVMT)]
```

    ## [1] 110857.1

As percentage

``` r
cams[AREA == 0, sum(SPPLIVMT)] / cams[, sum(SPPLIVMT)]
```

    ## [1] 0.100783

``` r
aa[AREA == 0, sum(SPPLIVMT)] / aa[, sum(SPPLIVMT)]
```

    ## [1] 0.192509
