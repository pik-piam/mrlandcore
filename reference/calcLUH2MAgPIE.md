# calcLUH2MAgPIE

Calculates the real aggregation of LUH croptypes to MAgPIE croptypes out
of LUH2FAO and FAO2MAgPIE mappings

## Usage

``` r
calcLUH2MAgPIE(
  share = "total",
  bioenergy = "ignore",
  rice = "non_flooded",
  missing = "ignore"
)
```

## Arguments

- share:

  total (for total numbers), LUHofMAG (for share of LUH within kcr
  types), MAGofLUH (for share of kcr within LUH types)

- bioenergy:

  "ignore": 0 for share and totals, "fix": fixes betr and begr shares in
  LUHofMAG to 1 for c3per and c4per

- rice:

  rice category: "non_flooded" or "total"

- missing:

  "ignore" will leave data as is, "fill" will add proxy values for data
  gaps of FAO

## Value

List of magpie objects with results on country level, weight on country
level, unit and description

## Author

Kristine Karstens, Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("LUH2MAgPIE")
} # }
```
