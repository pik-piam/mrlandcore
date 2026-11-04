# calcFallow

Returns fallow land calculated based on physical cropland extent and
harvested area output of default croparea datasource. The resultant
areas are scaled to match the land croparea land class of
calcLanduseInitialisation.

## Usage

``` r
calcFallow(cellular = FALSE)
```

## Arguments

- cellular:

  TRUE for cellular outputs

## Value

MAgPIE object containing fallow land in Mha

## See also

[`readLandInG`](readLandInG.md)

## Author

David Hoetten, Felicitas Beier, Benjamin Bodirsky

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("Fallow")
} # }
```
