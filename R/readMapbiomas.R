#' @title readMapBiomas
#' @description
#' Reads MapBiomas secondary vegetation time series data for Brazil and converts
#' it into a magpie object that reflects the original CSV structure.
#' Data was collected from the MapBiomas platform
#' (https://plataforma.brasil.mapbiomas.org/).
#'
#' @param subtype Data subtype (currently only "SecVeg")
#'
#' @return A magpie object with secondary vegetation classes for Brazil (ha)
#' @author Wanderson Costa, Alex Koberle, Miodrag Stevanovic
#' @examples
#' \dontrun{
#' readSource("MapBiomas", subtype="SecVeg")
#' }
#'
#' @importFrom utils read.csv

readMapBiomas <- function(subtype = "SecVeg") {

  files <- c(
    SecVeg = "Time series of Secondary Vegetation - Annual by class - 1985 - 2024.csv"
  )

  if (!subtype %in% names(files)) {
    stop("Unknown subtype: ", subtype)
  }

  # Read original CSV
  dat <- read.csv(
    file = files[subtype],
    stringsAsFactors = FALSE
  )

  # Minimal cleaning: replace missing values
  dat[dat == " - "] <- NA

  # Convert numeric columns (keep original structure)
  dat$Consolidated.secondary.vegetation <-
    as.numeric(gsub(",", "", dat$Consolidated.secondary.vegetation))

  dat$Recovery.to.Secondary.Vegetation <-
    as.numeric(gsub(",", "", dat$Recovery.to.Secondary.Vegetation))

  # Create magpie object (raw structure)
  mag <- as.magpie(
    x = dat[, c(
      "Year",
      "Consolidated.secondary.vegetation",
      "Recovery.to.Secondary.Vegetation"
    )],
    spatial  = "BRA",
    temporal = "Year"
  )
  getItems(mag, dim = "region") <- "BRA"

  return(mag)
}
