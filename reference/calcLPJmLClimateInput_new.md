# calcLPJmLClimateInput_new

Handle LPJmL climate input data and its time behaviour (smoothing and
harmonizing to baseline)

## Usage

``` r
calcLPJmLClimateInput_new(
  climatetype = "MRI-ESM2-0:ssp370",
  variable = "temperature:annualMean",
  stage = "harmonized2020",
  lpjmlVersion = "LPJmL4_for_MAgPIE_44ac93de"
)
```

## Arguments

- climatetype:

  Switch between different climate scenario

- variable:

  Switch between different climate inputs and temporal resolution

- stage:

  Degree of processing: raw, smoothed - raw or smoothed data from
  1930\|1951 raw1901, smoothed1901 - raw or smoothed data from 1901
  harmonized, harmonized2020 - based on toolLPJmLVersion

- lpjmlVersion:

  LPJmL Version hand over

## Value

magpie object in cellular resolution

## Author

Marcos Alves, Kristine Karstens, Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("LPJmLClimateInput_new",
           climatetype = "MRI-ESM2-0:ssp370",
           variable = "temperature:annualMean")
} # }
```
