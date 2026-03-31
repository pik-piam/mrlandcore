# calcLUH2v2

Integrates the LUH2v2 landuse-dataset

## Usage

``` r
calcLUH2v2(
  landuse_types = "magpie",
  irrigation = FALSE,
  cellular = FALSE,
  cells = "lpjcell",
  selectyears = "past_til2020"
)
```

## Arguments

- landuse_types:

  magpie: magpie landuse classes, LUH2v2: original landuse classes
  flooded: flooded areas as reported by LUH

- irrigation:

  if true: areas are returned separated by irrigated and rainfed, if
  false: total areas

- cellular:

  if true: dataset is returned on 0.5 degree resolution

- cells:

  Switch between "magpiecell" (59199) and "lpjcell" (67420) NOTE: This
  setting also affects the sums on country level!

- selectyears:

  years to be returned (default: "past")

## Value

List of magpie objects with results on country level, weight on country
level, unit and description

## See also

\[calcLanduseInitialisation()\]

## Author

Benjamin Leon Bodirsky, Florian Humpenoeder, Jens Heinke, Felicitas
Beier

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("LUH2v2")
} # }
```
