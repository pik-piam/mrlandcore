# readLandInG

Reads in LandInG data

## Usage

``` r
readLandInG(subtype = "physicalArea")
```

## Arguments

- subtype:

  Type of LandInG data that should be read:

  - `physicalArea`: Cropland extend/ physical cropping area separated in
    irrigated and rainfed

  - `harvestedArea`: Harvested area separated in different crop types

## Value

magpie object

## See also

[`readSource`](https://rdrr.io/pkg/madrat/man/readSource.html)

## Author

Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
A <- readSource("LandInG", subtype = "harvestedArea", aggregate = FALSE)
} # }
```
