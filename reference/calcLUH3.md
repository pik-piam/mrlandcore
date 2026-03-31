# calcLUH3

Prepares the LUH3 historic landuse-dataset for usage in MAgPIE.

## Usage

``` r
calcLUH3(
  landuseTypes = "magpie",
  irrigation = FALSE,
  cellular = FALSE,
  yrs = seq(1965, 2020, 5)
)
```

## Arguments

- landuseTypes:

  magpie: magpie landuse classes, LUH3: original landuse classes

- irrigation:

  if true: areas are returned separated by irrigated and rainfed, if
  false: total areas (irrigated + rainfed)

- cellular:

  if true: dataset is returned on 0.5 degree resolution, if false:
  return country-level data

- yrs:

  years to be returned

## Value

magpie object with land data in Mha

## See also

\[calcLanduseInitialisation()\]

## Author

Wanderson Costa, Pascal Sauer, Miodrag Stevanovic, Alexandre Koberle

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("LUH3")
} # }
```
