# calcMulticropping

calculates a multiple cropping factor based on area harvested, physical
cropland area (and optionally fallow land).

## Usage

``` r
calcMulticropping(extend_future = FALSE, factortype = "CI")
```

## Arguments

- extend_future:

  if TRUE

- factortype:

  CI: cropping intensity factor calculated as ratio of harvested to
  physical area where values above one indicate multicropping, below one
  fallow land (default) MC: multiple cropping factor indicating areas
  that are harvested more than once in one year calculated taking fallow
  land into account explicitly: harvestedArea / (physicalArea -
  fallowLand)

## Value

List of magpie objects with results on country level, weight on country
level, unit and description.

## See also

\[calcFAOLand()\], \[calcCroparea()\]

## Author

Benjamin Leon Bodirsky, David Chen, Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("Multicropping")
} # }
```
