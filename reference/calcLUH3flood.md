# calcLUH3flood

Prepares the LUH3 historic flood data for usage in MAgPIE, by
calculating the share of c3ann with c3ann area in Mha to obtain flood
area in Mha.

## Usage

``` r
calcLUH3flood(cellular = FALSE, yrs = seq(1965, 2020, 5))
```

## Arguments

- cellular:

  if true: dataset is returned on 0.5 degree resolution, if false:
  return country-level data

- yrs:

  years to be returned

## Value

magpie object with flood data in Mha

## See also

\[calcLanduseInitialisation()\]

## Author

Wanderson Costa, Pascal Sauer, Miodrag Stevanovic, Alexandre Koberle

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("LUH3flood")
} # }
```
