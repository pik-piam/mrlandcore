# calcMultipleCroppingZones

This function returns multiple cropping zones at 0.5 degree resolution

## Usage

``` r
calcMultipleCroppingZones(layers = 2)
```

## Arguments

- layers:

  8 for original GAEZ layers, 3 for aggregated multiple cropping zones
  with 1 = single cropping, 2 = double cropping, 3 = triple cropping 2
  for aggregated boolean multicropping potential with 0 = no
  multicropping (single cropping), 1 = multiple cropping

## Value

magpie object in cellular resolution

## Author

Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("MultipleCroppingZones", layers = 3, aggregate = FALSE)
} # }
```
