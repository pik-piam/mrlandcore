# calcLanduseInitialisationBase

Calculates the cellular MAgPIE landuse initialisation area. Data from
FAO on forestry is used to split the secondary forest pool of the LUH3
dataset into forestry and secdforest. This function returns the data set
in a basic configuration. Use
[`calcLanduseInitialisation`](calcLanduseInitialisation.md) for more
settings.

## Usage

``` r
calcLanduseInitialisationBase(cells = "lpjcell", selectyears = "past_til2020")
```

## Arguments

- cells:

  "magpiecell" for 59199 cells or "lpjcell" for 67420 cells

- selectyears:

  Years to be computed

## Value

Cellular landuse initialisation in its base configuration

## Author

Jan Philipp Dietrich, Benjamin Leon Bodirsky, Kristine Karstens,
Felcitas Beier, Patrick v. Jeetze

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("LanduseInitialisationBase")
} # }
```
