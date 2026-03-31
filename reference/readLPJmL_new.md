# readLPJmL_new

Read in LPJmL outputs

## Usage

``` r
readLPJmL_new(
  subtype = "LPJmL4_for_MAgPIE_44ac93de:GSWP3-W5E5:historical:soilc"
)
```

## Arguments

- subtype:

  Switch between different inputs (eg.
  "LPJmL5.2_Pasture:IPSL_CM6A_LR:ssp126_co2_limN_00:soilc_past_hist")

## Value

List of magpie objects with results on cellular level, weight, unit and
description.

## See also

\[readLPJ()\]

## Author

Kristine Karstens, Abhijeet Mishra, Felicitas Beier, Marcos Alves

## Examples

``` r
if (FALSE) { # \dontrun{
readSource("LPJmL_new", convert = FALSE)
} # }
```
