# calcMulticropping

Multicropping factor, telling how many harvests are produced on one unit
of physical area. Fallow lands are not counted in, so the factor is
always larger than 1.

## Usage

``` r
calcMulticropping(irrigation = TRUE)
```

## Arguments

- irrigation:

  report irrigated and non-irrigated areas separately

## Value

List of magpie objects with results on country level, weight on country
level, unit and description.

## See also

\[calcCroparea()\]

## Author

Benjamin Leon Bodirsky, Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("Multicropping")
} # }
```
