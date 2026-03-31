# calcGrassGPP

Calculates gross primary production (GPP) of grassland under irrigated
and rainfed conditions based on LPJmL inputs.

## Usage

``` r
calcGrassGPP(selectyears, lpjml, climatetype, season)
```

## Arguments

- selectyears:

  Years to be returned

- lpjml:

  LPJmL version required for respective inputs: natveg or crop

- climatetype:

  Switch between different climate scenarios or historical baseline
  "GSWP3-W5E5:historical"

- season:

  "wholeYear": grass GPP in the entire year (main + off season)
  "mainSeason": grass GPPP in the crop-specific growing period of LPJmL
  (main season)

## Value

magpie object in cellular resolution

## Author

Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("GrassGPP", aggregate = FALSE)
} # }
```
