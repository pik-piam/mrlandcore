# calcFallowLand

Calculates fallow land on grid cell level, based on physical cropland
extend and harvested area output of LandInG data. The formula "fallow
land are = max( physical cropland area - harvested cropland area, 0)" is
used. Due to multiple cropping, harvested cropland area can be greater
than non-fallow land area and even greater than physical cropland area.
Thus, the results can only be considered a rough estimate of fallow land
area.

## Usage

``` r
calcFallowLand(cellular = TRUE)
```

## Arguments

- cellular:

  TRUE for cellular outputs.

## Value

MAgPIE object containing fallow land in Mha

## See also

[`readLandInG`](readLandInG.md)

## Author

David Hoetten, Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("FallowLand")
} # }
```
