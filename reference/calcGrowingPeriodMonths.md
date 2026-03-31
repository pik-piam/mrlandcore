# calcGrowingPeriodMonths

Calculates which gridcell-specific months in which growing conditions
are favorable for crop growth based on monthly grass GPP

## Usage

``` r
calcGrowingPeriodMonths(selectyears, lpjml, climatetype, minThreshold = 100)
```

## Arguments

- selectyears:

  Years to be returned

- lpjml:

  LPJmL version required for respective inputs: natveg or crop

- climatetype:

  Switch between different climate scenarios or historical baseline
  "GSWP3-W5E5:historical"

- minThreshold:

  Threshold of monthly grass GPP to be classified as growing period
  month Unit of the threshold is gC/m^2. Default: 100gC/m^2 Note: the
  default value is chosen based on LPJmL version 5 to reflect multiple
  cropping suitability as shown in GAEZ-4. An update of LPJmL5 with
  regards to grass management may require an adjustment of the
  threshold.

## Value

magpie object in cellular resolution

## Author

Felicitas Beier, Jens Heinke

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("GrowingPeriodMonths", aggregate = FALSE)
} # }
```
