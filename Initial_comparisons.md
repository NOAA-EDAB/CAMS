Initial Comparisons
================
Sean Lucey
2/03/2022

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

    ## [1] 53227

``` r
nrow(aa)
```

    ## [1] 57664

Total landings

``` r
cams[, sum(SPPLIVMT)]
```

    ## [1] 571748.4

``` r
aa[, sum(SPPLIVMT)]
```

    ## [1] 575854.3

Landings that are missing area

``` r
cams[AREA == 0, sum(SPPLIVMT)]
```

    ## [1] 11146.28

``` r
aa[AREA == 0, sum(SPPLIVMT)]
```

    ## [1] 110857.1

As percentage

``` r
cams[AREA == 0, sum(SPPLIVMT)] / cams[, sum(SPPLIVMT)]
```

    ## [1] 0.01949508

``` r
aa[AREA == 0, sum(SPPLIVMT)] / aa[, sum(SPPLIVMT)]
```

    ## [1] 0.192509

Now to run the scripts for the SOE. First assign landings to an EPU then
aggregate landings by SOE groups.

``` r
load(here('data-raw', 'SOE_species_list.RData'))

#AA
#Assign areas based on mskeyAreas
aa <- aggregate_area(aa, userAreas = comlandr::mskeyAreas, areaDescription = 'EPU',
                     propDescription = 'MeanProp')

#Aggregate by EBFM codes
aa.agg <- merge(aa, unique(species[!is.na(NESPP3), list(NESPP3, SOE.20, Fed.Managed)]), 
                by = 'NESPP3', all.x = T)

#Fix NA codes
aa.agg[is.na(SOE.20), SOE.20 := 'Other']

#Sum Landings
aa.land <- aa.agg[, sum(SPPLIVMT), by = c('YEAR', 'EPU', 'SOE.20', 'Fed.Managed')]
aa.land[, Total := sum(V1), by = c('YEAR', 'EPU', 'SOE.20')]
aa.land[, Prop.managed := V1 / Total]
setnames(aa.land, 'V1', 'SPPLIVMT')

aa.land
```

    ##     YEAR   EPU        SOE.20 Fed.Managed     SPPLIVMT       Total Prop.managed
    ##  1: 2019    GB         Other        <NA> 4.594658e+02   459.51255 9.998983e-01
    ##  2: 2019   MAB         Other        <NA> 2.110531e+02   211.40512 9.983348e-01
    ##  3: 2019    GB     Piscivore       JOINT 3.133564e+03 11788.82543 2.658080e-01
    ##  4: 2019   GOM     Piscivore       JOINT 1.595248e+03  4039.95722 3.948676e-01
    ##  5: 2019 Other     Piscivore       JOINT 5.824381e+00  1009.79159 5.767904e-03
    ##  6: 2019   MAB     Piscivore       JOINT 1.863366e+03 17113.33113 1.088839e-01
    ##  7: 2019    GB     Piscivore       MAFMC 3.452690e+03 11788.82543 2.928782e-01
    ##  8: 2019   GOM     Piscivore       MAFMC 1.451662e+01  4039.95722 3.593260e-03
    ##  9: 2019 Other     Piscivore       MAFMC 7.121192e+02  1009.79159 7.052140e-01
    ## 10: 2019   MAB     Piscivore       MAFMC 1.213230e+04 17113.33113 7.089385e-01
    ## 11: 2019   GOM         Other        <NA> 2.474785e+02   247.47849 1.000000e+00
    ## 12: 2019 Other         Other        <NA> 2.916294e+01    29.16316 9.999926e-01
    ## 13: 2019    GB   Planktivore       MAFMC 4.030146e+02  2160.92753 1.865008e-01
    ## 14: 2019   GOM   Planktivore       MAFMC 8.709155e+01  1779.26011 4.894818e-02
    ## 15: 2019 Other   Planktivore       MAFMC 5.625248e+01    56.25305 9.999899e-01
    ## 16: 2019   MAB   Planktivore       MAFMC 1.708557e+03  1741.02037 9.813538e-01
    ## 17: 2019    GB     Piscivore       NEFMC 5.182375e+03 11788.82543 4.396007e-01
    ## 18: 2019   GOM     Piscivore       NEFMC 2.428802e+03  4039.95722 6.011949e-01
    ## 19: 2019 Other     Piscivore       NEFMC 2.869126e+02  1009.79159 2.841305e-01
    ## 20: 2019   MAB     Piscivore       NEFMC 3.100954e+03 17113.33113 1.812011e-01
    ## 21: 2019    GB    Benthivore        <NA> 3.117710e+03  6052.44710 5.151156e-01
    ## 22: 2019   MAB    Benthivore        <NA> 4.826861e+03  6799.90061 7.098429e-01
    ## 23: 2019   GOM    Benthivore        <NA> 4.557323e+02  3914.21721 1.164300e-01
    ## 24: 2019    GB   Planktivore        <NA> 3.593163e+01  2160.92753 1.662787e-02
    ## 25: 2019   GOM   Planktivore        <NA> 8.256335e+00  1779.26011 4.640319e-03
    ## 26: 2019 Other   Planktivore        <NA> 5.679221e-04    56.25305 1.009585e-05
    ## 27: 2019   MAB   Planktivore        <NA> 2.778046e+01  1741.02037 1.595642e-02
    ## 28: 2019    GB    Benthivore       NEFMC 2.641797e+03  6052.44710 4.364842e-01
    ## 29: 2019   GOM    Benthivore       NEFMC 3.454845e+03  3914.21721 8.826400e-01
    ## 30: 2019 Other    Benthivore       NEFMC 2.174834e+01   557.19775 3.903164e-02
    ## 31: 2019   MAB    Benthivore       NEFMC 4.311019e+02  6799.90061 6.339827e-02
    ## 32: 2019    GB     Piscivore        <NA> 2.019545e+01 11788.82543 1.713101e-03
    ## 33: 2019   GOM     Piscivore        <NA> 1.390686e+00  4039.95722 3.442328e-04
    ## 34: 2019   MAB     Piscivore        <NA> 1.671189e+01 17113.33113 9.765419e-04
    ## 35: 2019    GB   Planktivore       NEFMC 1.721981e+03  2160.92753 7.968714e-01
    ## 36: 2019   GOM   Planktivore       NEFMC 1.683912e+03  1779.26011 9.464115e-01
    ## 37: 2019   MAB   Planktivore       NEFMC 4.682970e+00  1741.02037 2.689785e-03
    ## 38: 2019 Other     Piscivore        <NA> 4.935461e+00  1009.79159 4.887603e-03
    ## 39: 2019    GB    Benthivore       MAFMC 2.929397e+02  6052.44710 4.840021e-02
    ## 40: 2019   GOM    Benthivore       MAFMC 3.640290e+00  3914.21721 9.300173e-04
    ## 41: 2019 Other    Benthivore       MAFMC 7.259585e-01   557.19775 1.302874e-03
    ## 42: 2019   MAB    Benthivore       MAFMC 1.541937e+03  6799.90061 2.267588e-01
    ## 43: 2019 Other    Benthivore        <NA> 5.347234e+02   557.19775 9.596655e-01
    ## 44: 2019    GB Apex Predator        <NA> 7.927803e+01    79.27803 1.000000e+00
    ## 45: 2019   GOM Apex Predator        <NA> 1.353009e+01    13.53009 1.000000e+00
    ## 46: 2019 Other Apex Predator        <NA> 2.131694e+01    21.31694 1.000000e+00
    ## 47: 2019   MAB Apex Predator        <NA> 1.535655e+02   153.56546 1.000000e+00
    ## 48: 2019    GB         Other       MAFMC 4.674010e-02   459.51255 1.017167e-04
    ## 49: 2019 Other         Other       MAFMC 2.165992e-04    29.16316 7.427151e-06
    ## 50: 2019   MAB         Other       MAFMC 3.520370e-01   211.40512 1.665224e-03
    ## 51: 2019    GB       Benthos       MAFMC 7.466716e+03 17299.98992 4.316024e-01
    ## 52: 2019 Other       Benthos       MAFMC 1.546243e+02   257.64772 6.001385e-01
    ## 53: 2019   MAB       Benthos       MAFMC 2.477529e+03  8603.23696 2.879764e-01
    ## 54: 2019   GOM       Benthos       MAFMC 2.161195e+01   583.45267 3.704148e-02
    ## 55: 2019    GB       Benthos        <NA> 1.612854e+02 17299.98992 9.322859e-03
    ## 56: 2019   GOM       Benthos        <NA> 1.188335e+01   583.45267 2.036730e-02
    ## 57: 2019 Other       Benthos        <NA> 5.153974e-03   257.64772 2.000396e-05
    ## 58: 2019   MAB       Benthos        <NA> 1.945974e+01  8603.23696 2.261909e-03
    ## 59: 2019    GB       Benthos       NEFMC 9.671988e+03 17299.98992 5.590748e-01
    ## 60: 2019   GOM       Benthos       NEFMC 5.499574e+02   583.45267 9.425912e-01
    ## 61: 2019 Other       Benthos       NEFMC 1.030183e+02   257.64772 3.998415e-01
    ## 62: 2019   MAB       Benthos       NEFMC 6.106248e+03  8603.23696 7.097617e-01
    ##     YEAR   EPU        SOE.20 Fed.Managed     SPPLIVMT       Total Prop.managed

``` r
#CAMS
#Assign areas based on mskeyAreas
cams <- aggregate_area(cams, userAreas = comlandr::mskeyAreas, areaDescription = 'EPU',
                       propDescription = 'MeanProp')

#Aggregate by EBFM codes
cams.agg <- merge(cams, unique(species[!is.na(NESPP3), 
                                       list(NESPP3, SOE.20, Fed.Managed)]),
                  by = 'NESPP3', all.x = T)

#Fix NA codes
cams.agg[is.na(SOE.20), SOE.20 := 'Other']

#Sum Landings
cams.land <- cams.agg[, sum(SPPLIVMT), by = c('YEAR', 'EPU', 'SOE.20', 'Fed.Managed')]
cams.land[, Total := sum(V1), by = c('YEAR', 'EPU', 'SOE.20')]
cams.land[, Prop.managed := V1 / Total]
setnames(cams.land, 'V1', 'SPPLIVMT')

cams.land
```

    ##     YEAR   EPU        SOE.20 Fed.Managed     SPPLIVMT       Total Prop.managed
    ##  1: 2019    GB         Other        <NA> 6.653794e+02   665.45315 9.998892e-01
    ##  2: 2019   MAB         Other        <NA> 2.095239e+02   210.07931 9.973563e-01
    ##  3: 2019    GB     Piscivore       JOINT 3.221018e+03 12328.80290 2.612596e-01
    ##  4: 2019   GOM     Piscivore       JOINT 1.608350e+03  4036.13316 3.984879e-01
    ##  5: 2019 Other     Piscivore       JOINT 5.919133e+00  1109.90914 5.332989e-03
    ##  6: 2019   MAB     Piscivore       JOINT 2.039561e+03 17275.14295 1.180633e-01
    ##  7: 2019    GB     Piscivore       MAFMC 3.766868e+03 12328.80290 3.055340e-01
    ##  8: 2019   GOM     Piscivore       MAFMC 1.393363e+01  4036.13316 3.452222e-03
    ##  9: 2019 Other     Piscivore       MAFMC 7.982547e+02  1109.90914 7.192073e-01
    ## 10: 2019   MAB     Piscivore       MAFMC 1.199584e+04 17275.14295 6.943991e-01
    ## 11: 2019   GOM         Other        <NA> 2.898425e+02   289.84248 1.000000e+00
    ## 12: 2019 Other         Other        <NA> 2.605491e+01    26.05513 9.999917e-01
    ## 13: 2019    GB   Planktivore       MAFMC 4.252036e+02  2169.04945 1.960322e-01
    ## 14: 2019   GOM   Planktivore       MAFMC 9.690338e+01  1800.77106 5.381216e-02
    ## 15: 2019 Other   Planktivore       MAFMC 5.593402e+01    55.93458 9.999898e-01
    ## 16: 2019   MAB   Planktivore       MAFMC 1.745145e+03  1754.38398 9.947339e-01
    ## 17: 2019    GB     Piscivore       NEFMC 5.246599e+03 12328.80290 4.255562e-01
    ## 18: 2019   GOM     Piscivore       NEFMC 2.410067e+03  4036.13316 5.971227e-01
    ## 19: 2019 Other     Piscivore       NEFMC 3.011325e+02  1109.90914 2.713128e-01
    ## 20: 2019   MAB     Piscivore       NEFMC 3.214347e+03 17275.14295 1.860677e-01
    ## 21: 2019    GB    Benthivore        <NA> 3.573850e+03  6831.10282 5.231732e-01
    ## 22: 2019   GOM    Benthivore        <NA> 4.148552e+02  3876.69875 1.070125e-01
    ## 23: 2019   MAB    Benthivore        <NA> 5.015950e+03  7118.67597 7.046185e-01
    ## 24: 2019    GB   Planktivore        <NA> 1.883799e+01  2169.04945 8.684905e-03
    ## 25: 2019   GOM   Planktivore        <NA> 1.725876e+01  1800.77106 9.584094e-03
    ## 26: 2019 Other   Planktivore        <NA> 5.679221e-04    55.93458 1.015333e-05
    ## 27: 2019    GB    Benthivore       NEFMC 2.784454e+03  6831.10282 4.076141e-01
    ## 28: 2019   GOM    Benthivore       NEFMC 3.454697e+03  3876.69875 8.911441e-01
    ## 29: 2019 Other    Benthivore       NEFMC 7.975059e+01   645.34242 1.235787e-01
    ## 30: 2019   MAB    Benthivore       NEFMC 4.388787e+02  7118.67597 6.165174e-02
    ## 31: 2019    GB     Piscivore        <NA> 9.431782e+01 12328.80290 7.650201e-03
    ## 32: 2019   GOM     Piscivore        <NA> 3.782443e+00  4036.13316 9.371454e-04
    ## 33: 2019   MAB     Piscivore        <NA> 2.539117e+01 17275.14295 1.469809e-03
    ## 34: 2019    GB   Planktivore       NEFMC 1.725008e+03  2169.04945 7.952829e-01
    ## 35: 2019   GOM   Planktivore       NEFMC 1.686609e+03  1800.77106 9.366037e-01
    ## 36: 2019   MAB   Planktivore       NEFMC 5.660129e+00  1754.38398 3.226277e-03
    ## 37: 2019 Other     Piscivore        <NA> 4.602726e+00  1109.90914 4.146939e-03
    ## 38: 2019   MAB   Planktivore        <NA> 3.578571e+00  1754.38398 2.039788e-03
    ## 39: 2019    GB    Benthivore       MAFMC 4.727990e+02  6831.10282 6.921269e-02
    ## 40: 2019   GOM    Benthivore       MAFMC 7.146145e+00  3876.69875 1.843358e-03
    ## 41: 2019 Other    Benthivore       MAFMC 1.297798e-01   645.34242 2.011022e-04
    ## 42: 2019   MAB    Benthivore       MAFMC 1.663847e+03  7118.67597 2.337298e-01
    ## 43: 2019 Other    Benthivore        <NA> 5.654620e+02   645.34242 8.762202e-01
    ## 44: 2019    GB Apex Predator        <NA> 3.069636e+02   306.96358 1.000000e+00
    ## 45: 2019   GOM Apex Predator        <NA> 2.470536e+01    24.70536 1.000000e+00
    ## 46: 2019 Other Apex Predator        <NA> 1.989314e+01    19.89314 1.000000e+00
    ## 47: 2019   MAB Apex Predator        <NA> 1.028203e+02   102.82033 1.000000e+00
    ## 48: 2019    GB         Other       MAFMC 7.375803e-02   665.45315 1.108388e-04
    ## 49: 2019 Other         Other       MAFMC 2.165992e-04    26.05513 8.313112e-06
    ## 50: 2019   MAB         Other       MAFMC 5.553946e-01   210.07931 2.643738e-03
    ## 51: 2019    GB       Benthos       MAFMC 7.241772e+03 17069.75118 4.242459e-01
    ## 52: 2019 Other       Benthos       MAFMC 1.544451e+02   257.83216 5.990142e-01
    ## 53: 2019   MAB       Benthos       MAFMC 2.483123e+03  8698.98667 2.854496e-01
    ## 54: 2019   GOM       Benthos       MAFMC 2.160175e+01   593.15200 3.641857e-02
    ## 55: 2019    GB       Benthos        <NA> 1.501877e+02 17069.75118 8.798468e-03
    ## 56: 2019   MAB       Benthos        <NA> 1.715660e+01  8698.98667 1.972253e-03
    ## 57: 2019   GOM       Benthos        <NA> 1.444519e+01   593.15200 2.435326e-02
    ## 58: 2019    GB       Benthos       NEFMC 9.677791e+03 17069.75118 5.669556e-01
    ## 59: 2019   GOM       Benthos       NEFMC 5.571051e+02   593.15200 9.392282e-01
    ## 60: 2019 Other       Benthos       NEFMC 1.033870e+02   257.83216 4.009858e-01
    ## 61: 2019   MAB       Benthos       NEFMC 6.198707e+03  8698.98667 7.125781e-01
    ##     YEAR   EPU        SOE.20 Fed.Managed     SPPLIVMT       Total Prop.managed

Graphical differences

``` r
#Merge landings
all <- merge(aa.land, cams.land, by = c('YEAR', 'EPU', 'SOE.20', 'Fed.Managed'))
setnames(all, c('SPPLIVMT.x', 'Total.x', 'Prop.managed.x', 
                'SPPLIVMT.y', 'Total.y', 'Prop.managed.y'),
         c('AA.SPPLIVMT',   'AA.Total',   'AA.Prop.managed',
           'CAMS.SPPLIVMT', 'CAMS.Total', 'CAMS.Prop.managed'))

tot.land <- unique(all[, list(YEAR, EPU, SOE.20, AA.Total, CAMS.Total)], 
                   by = c('YEAR', 'EPU', 'SOE.20'))

#Put in long form
tot.land.long <- data.table::melt(tot.land, id.vars = c('YEAR', 'EPU', 'SOE.20'))

#Plot as a barplot
land.bar <- ggplot(data = tot.land.long, 
                   aes(SOE.20, value, fill = variable)) +
    geom_bar(stat = "identity", position = 'dodge') +
    facet_grid(rows = 'EPU', scales = "free")

plot(land.bar)
```

![](Initial_comparisons_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->
