# calcForestArea

Calculates consistent forest area and its subcategories based on
FAO_FRA2015 (and FAO_FRA2020 only for forest plantations) and
LanduseInitialisation data.

## Usage

``` r
calcForestArea(selectyears = "past_til2020")
```

## Arguments

- selectyears:

  passed to magpiesets::findset

## Value

List of magpie object with results on country level, weight, unit and
description.

## Author

Kristine Karstens, Jan Philipp Dietrich

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("ForestArea")
} # }
```
