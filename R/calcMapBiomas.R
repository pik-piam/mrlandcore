#' @title calcMapBiomas
#' @description
#' Calculates total secondary vegetation area for Brazil based on MapBiomas data.
#' Consolidated and recovery secondary vegetation are aggregated, selected years
#' are kept, and values are converted from hectares to million hectares (Mha).
#'
#' @param subtype Data subtype (currently only "SecVeg")
#'
#' @return A magpie object with total secondary vegetation area for Brazil (Mha)
#' @author Wanderson Costa, Alex Koberle, Miodrag Stevanovic
#' @examples
#' \dontrun{
#' calcOutput("MapBiomas", subtype="SecVeg")
#' }

calcMapBiomas <- function(subtype = "SecVeg") {

  # Read raw MapBiomas data
  mag <- readSource("MapBiomas", subtype = subtype)

  # Aggregate secondary vegetation classes
  mag <- dimSums(mag, dim = 3)
  names(dimnames(mag))[3] <- "variable"
  getItems(mag, dim = "variable") <- "SecVeg"

  # Keep only selected years
  years <- c(1995, 2000, 2005, 2010, 2015, 2020)
  mag <- mag[, paste0("y", years), , drop = FALSE]

  # Convert from ha to Mha
  mag <- mag / 1e6

  # years to add before 1995
  missingYears <- seq(1965, 1990, by = 5)

  # repeat the value of y1995 and set the Year dimnames before binding
  magFilled <- mag[, "y1995", , drop = FALSE][, rep(1, length(missingYears)), , drop = FALSE]
  dimnames(magFilled)$Year <- paste0("y", missingYears)

  # combine with the existing magpie
  out <- mbind(magFilled, mag)

  return(list(x = out,
              isocountries = FALSE,
              weight = NULL,
              unit = "Mha",
              description = "Total secondary vegetation area for Brazil based on MapBiomas data"))
}
