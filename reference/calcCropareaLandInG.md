# calcCropareaLandInG

This function uses total physical area and crop-specific harvested area
data from LandInG to calculate crop-specific physical and harvested
areas considering special rules for the allocation of perennial and
annual crops.

## Usage

``` r
calcCropareaLandInG(
  sectoral = "kcr",
  physical = TRUE,
  cellular = FALSE,
  cells = "magpiecell",
  irrigation = FALSE,
  selectyears = "all",
  lpjml = c(natveg = "LPJmL4_for_MAgPIE_44ac93de", crop =
    "ggcmi_phase3_nchecks_bft_e511ac58"),
  climatetype = "GSWP3-W5E5:historical"
)
```

## Arguments

- sectoral:

  "kcr" MAgPIE items, and "lpj" LPJmL items

- physical:

  if TRUE the sum over all crops plus fallow land (of calcFallowLand)
  agrees with the physical cropland of readLandInG(subtype = physical)

- cellular:

  if TRUE: calculates cellular crop area for all magpie croptypes.
  Option FALSE is not (yet) available.

- cells:

  Switch between "magpiecell" (59199) and "lpjcell" (67420)

- irrigation:

  If true: cellular areas are returned separated into irrigated and
  rainfed

- selectyears:

  extract certain years from the data

- lpjml:

  LPJmL version used to determine multiple cropping suitability

- climatetype:

  Climate scenario or historical baseline "GSWP3-W5E5:historical" used
  to determine multiple cropping suitability

## Value

MAgPIE object with cropareas

## Author

David Hoetten, Felicitas Beier
