# calcCroparea

returns croparea

## Usage

``` r
calcCroparea(
  sectoral = "kcr",
  physical = TRUE,
  fallow = FALSE,
  cellular = FALSE,
  irrigation = FALSE,
  datasource = "LandInG"
)
```

## Arguments

- sectoral:

  "kcr" MAgPIE items, and "lpj" LPJmL items

- physical:

  if TRUE it returns the physical area, with cropareas of multicropping
  systems being scaled to match physical areas, if FALSE the area
  harvested is returned

- fallow:

  if TRUE fallow land is returned as element in crop set

- cellular:

  if TRUE: calculates cellular MAgPIE crop area for all magpie
  croptypes. Crop area from LUH3 crop types (c3ann, c4ann, c3per, c4per,
  cnfx) are mapped to MAgpIE crop types using "FAO2LUH2MAG_croptypes"
  and doing an intermediate step via harvested areas of FAO weight area
  within a specific LUH crop type to divide into MAgPIE crop types.

- irrigation:

  If true: cellular areas are returned separated into irrigated and
  rainfed (see setup in calcLUH3)

- datasource:

  Croparea data source (LandInG or FAOLUH). Datasource FAOLUH is
  deprecated and is only kept for backwards compatibility.

## Value

areas of individual crops from FAOSTAT and weight

## Author

Felicitas Beier, Benjamin Leon Bodirsky
