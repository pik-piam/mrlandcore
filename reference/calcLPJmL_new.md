# calcLPJmL_new

Handle LPJmL data and its time behaviour (smoothing and harmonizing to
baseline)

## Usage

``` r
calcLPJmL_new(
  version = "LPJmL4_for_MAgPIE_44ac93de",
  climatetype = "MRI-ESM2-0:ssp370",
  subtype = "soilc",
  subdata = NULL,
  stage = "harmonized2020"
)
```

## Arguments

- version:

  Switch between LPJmL versions (including addons for further version
  specification)

- climatetype:

  Switch between different climate scenarios

- subtype:

  Switch between different lpjml input as specified in readLPJmL

- subdata:

  Switch between data dimension subitems

- stage:

  Degree of processing: raw, smoothed - raw or smoothed data from
  1930\|1951 raw1901, smoothed1901 - raw or smoothed data from 1901
  harmonized, harmonized2020 - based on toolLPJmLVersion

## Value

List of magpie objects with results on cellular level, weight, unit and
description.

## See also

\[readLPJmL()\]

## Author

Kristine Karstens, Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("LPJmL_new", subtype = "soilc", aggregate = FALSE)
} # }
```
