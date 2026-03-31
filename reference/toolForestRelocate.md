# toolForestRelocate

Reallocates cellular forest information to better match FAO forest
information

## Usage

``` r
toolForestRelocate(lu, luCountry, natTarget, vegC)
```

## Arguments

- lu:

  uncorrected landuse initialisation data set (cell level)

- luCountry:

  uncorrected landuse initialisation on country level

- natTarget:

  target natural land allocation on country level

- vegC:

  vegetation carbon data used as reallocation weight

## Value

List of magpie object with results on cellular level

## Author

Kristine Karstens, Jan Philipp Dietrich, Felicitas Beier, Patrick v.
Jeetze
