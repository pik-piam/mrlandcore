# downloadLPJmL_new

Download LPJmL content by version, climate model and scenario

## Usage

``` r
downloadLPJmL_new(
  subtype = "LPJmL4_for_MAgPIE_44ac93de:GSWP3-W5E5:historical:soilc"
)
```

## Arguments

- subtype:

  Switch between different input It consists of LPJmL version, climate
  model, scenario and variable. For pasture lpjml runs, the scenario
  variable is used to navigate the output folder structure (e.g.
  'LPJmL4_for_MAgPIE_3dda0615:GSWP3-W5E5:historical:soilc' or
  "LPJmL5.2_Pasture:IPSL_CM6A_LR:ssp126_co2_limN_00:soilc_past_hist")

## Value

metadata entry

## Author

Kristine Karstens, Marcos Alves, Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
readSource("LPJmL_new", convert = FALSE)
} # }
```
