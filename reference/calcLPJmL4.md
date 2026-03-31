# calcLPJmL4

Handle LPJmL data and its time behaviour (averaging, approximation,
harmonizing to baseline)

## Usage

``` r
calcLPJmL4(
  version = "LPJmL4",
  climatetype = "CRU_4",
  subtype = "soilc",
  subdata = NULL,
  time = "raw",
  averaging_range = NULL,
  dof = NULL,
  harmonize_baseline = FALSE,
  ref_year = "y2015",
  limited = TRUE,
  hard_cut = FALSE,
  selectyears = "all"
)
```

## Arguments

- version:

  Switch between LPJmL4 and LPJmL4

- climatetype:

  Switch between different climate scenarios (default: "CRU_4")

- subtype:

  Switch between different lpjml input as specified in readLPJmL

- subdata:

  Switch between data dimension subitems

- time:

  average, spline or raw (default)

- averaging_range:

  just specify for time=="average": number of time steps to average

- dof:

  just specify for time=="spline": degrees of freedom

- harmonize_baseline:

  FALSE (default) nothing happens, if a baseline is specified here data
  is harmonized to that baseline (from ref_year on)

- ref_year:

  just specify for harmonize_baseline != FALSE : Reference year

- limited:

  just specify for harmonize_baseline != FALSE : if TRUE limited
  approached is used

- hard_cut:

  just specify for harmonize_baseline != FALSE : use hard cut instead of
  multiplicative factor

- selectyears:

  defaults to all years available

## Value

List of magpie objects with results on cellular level, weight, unit and
description.

## Author

Kristine Karstens, Felicitas Beier
