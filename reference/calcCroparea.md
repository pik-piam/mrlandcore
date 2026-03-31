# calcCroparea

Returns harvested areas of individual crops from FAOSTAT. Total
harvested areas can be lower or higher than arable land because of
multicropping or fallow land. Rice areas are distributed to flooded LUH
areas. Additional FAOSTAT rice areas are distributed based on country
shares.

## Usage

``` r
calcCroparea(
  sectoral = "kcr",
  physical = TRUE,
  cellular = FALSE,
  irrigation = FALSE
)
```

## Arguments

- sectoral:

  "area_harvested" returns croparea aggregated to FAO products,
  "ProductionItem" unaggregated ProdSTAT items, "FoodBalanceItem" Food
  Balance Sheet categories, "kcr" MAgPIE items, and "lpj" LPJmL items

- physical:

  if TRUE the sum over all crops agrees with the cropland area per
  country

- cellular:

  if TRUE: calculates cellular MAgPIE crop area for all magpie
  croptypes. Crop area from LUH3 crop types (c3ann, c4ann, c3per, c4per,
  cnfx) are mapped to MAgpIE crop types using "FAO2LUH2MAG_croptypes"
  and doing an intermediate step via harvested areas of FAO weight area
  within a specific LUH crop type to divide into MAgPIE crop types.

- irrigation:

  If true: cellular areas are returned separated into irrigated and
  rainfed (see setup in calcLUH3)

## Value

areas of individual crops from FAOSTAT and weight

## Author

Ulrich Kreidenweis, Kristine Karstens, Felicitas Beier
