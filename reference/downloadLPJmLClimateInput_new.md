# downloadLPJmLClimateInput_new

Download GCM climate input used for LPJmL runs

## Usage

``` r
downloadLPJmLClimateInput_new(
  subtype = "ISIMIP3bv2:MRI-ESM2-0:ssp370:temperature"
)
```

## Arguments

- subtype:

  Switch between different inputs (e.g.
  "ISIMIP3b:IPSL-CM6A-LR:historical:1850-2014:temperature") Argument
  consists of GCM version, climate model, scenario and variable,
  separated by ":"

## Value

metadata entry

## Author

Marcos Alves, Kristine Karstens

## Examples

``` r
if (FALSE) { # \dontrun{
readSource("LPJmLClimateInput_new", convert = "onlycorrect")
} # }
```
