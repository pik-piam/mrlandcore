# calcLanduseInitialisation

Calculates the cellular MAgPIE landuse initialisation area. Data from
FAO on forestry is used to split the secondary forest pool of the LU2v2
dataset into forestry and secd_forest.

## Usage

``` r
calcLanduseInitialisation(
  cellular = FALSE,
  nclasses = "seven",
  cells = "lpjcell",
  selectyears = "past_til2020",
  input_magpie = FALSE
)
```

## Arguments

- cellular:

  cellular (TRUE) or country-level/regional (FALSE) data? For
  country-level vs regional data: remember to set "aggregate" to FALSE.

- nclasses:

  options are either "six", "seven" or "nine".

  - "six" includes the original land use classes "crop", "past",
    "forestry", "forest", "urban" and "other"

  - "seven" separates primary and secondary forest and includes "crop",
    "past", "forestry", "primforest", "secdforest", "urban" and "other"

  - "nine" adds the separation of pasture and rangelands, as well as a
    differentiation of primary and secondary non-forest vegetation and
    therefore returns "crop", "past", "range", "forestry", "primforest",
    "secdforest", "urban", "primother" and "secdother"

- cells:

  if cellular is TRUE: "magpiecell" for 59199 cells or "lpjcell" for
  67420 cells

- selectyears:

  default on "past"

- input_magpie:

  applies area fix (set cells with zero area to minimal value to not
  disturb aggregating to clusters)

## Value

List of magpie object with results on country or cellular level, weight
on cellular level, unit and description.

## Author

Jan Philipp Dietrich, Benjamin Leon Bodirsky, Kristine Karstens,
Felcitas Beier, Patrick v. Jeetze

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("LanduseInitialisation")
} # }
```
