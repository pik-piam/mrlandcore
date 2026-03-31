# readGAEZv4

Read in data from the Global Agro-ecological Zones (GAEZ) data set
version 4

## Usage

``` r
readGAEZv4(subtype = "mci_CRUTS32_Hist_0010.tif")
```

## Arguments

- subtype:

  Name of GAEZ file to be read Available are for example:
  "mci_CRUTS32_Hist_0010.tif" (irrigated multiple cropping suitability
  for years 2000-2010), "mcr_CRUTS32_Hist_0010.tif" (rainfed multiple
  cropping suitability for years 2000-2010)

## Value

MAgPIE object at 0.5 cellular level

## Author

Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
readSource("GAEZv4", convert = "onlycorrect")
} # }
```
