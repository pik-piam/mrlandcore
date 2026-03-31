# calcMulticroppingSuitability

Calculates which grid cells are potentially suitable for multiple
cropping activities under rainfed and irrigated conditions. Calculation
is based on the length of the growing period determined by monthly
grassland gross primary production (GPP).

## Usage

``` r
calcMulticroppingSuitability(
  selectyears,
  lpjml,
  climatetype,
  suitability = "endogenous",
  sectoral = "kcr"
)
```

## Arguments

- selectyears:

  Years to be returned

- lpjml:

  LPJmL version required for respective inputs: natveg or crop

- climatetype:

  Switch between different climate scenarios or historical baseline
  "GSWP3-W5E5:historical"

- suitability:

  "endogenous": suitability for multiple cropping determined by rules
  based on grass and crop productivity "exogenous": suitability for
  multiple cropping given by GAEZ data set

- sectoral:

  "kcr" MAgPIE crops, and "lpj" LPJmL crops

## Value

magpie object in cellular resolution

## Author

Felicitas Beier, Jens Heinke

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("MulticroppingSuitability", aggregate = FALSE)
} # }
```
