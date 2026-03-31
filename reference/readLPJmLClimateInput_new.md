# readLPJmLClimateInput_new

Read Climate data used as LPJmL inputs into MAgPIE objects

## Usage

``` r
readLPJmLClimateInput_new(
  subset = "annualMean",
  subtype = "ISIMIP3bv2:MRI-ESM2-0:ssp370:temperature"
)
```

## Arguments

- subset:

  Switch between different subsets of the same subtype Available options
  are: "annualMean", "annualSum", "monthlyMean", "monthlySum",
  "wetDaysMonth" Note that not all subtype-subset combinations make
  sense

- subtype:

  Switch between different inputs, e.g.
  "ISIMIP3bv2:MRI-ESM2-0:ssp370:1850-2014:tas" Available variables are:
  \* tas - \* wet - \* per -

## Value

MAgPIE objects with results on cellular level.

## See also

`readLPJmLClimateInput_new`

## Author

Marcos Alves, Kristine Karstens, Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
readSource("LPJmLClimateInput_new", subtype, convert = "onlycorrect")
} # }
```
