#' @title calcMapBiomas
#' @description
#' Calculates total secondary vegetation area for Brazil based on MapBiomas data.
#' Consolidated and recovery secondary vegetation are aggregated, selected years
#' are kept, and values are converted from hectares to million hectares (Mha).
#'
#' @param subtype Data subtype (currently only "SecVeg")
#'
#' @return A magpie object with total secondary vegetation area for Brazil (Mha)
#' @author Wanderson Costa, Alex Koberle, Miodrag Stevanovic, Pascal Sauer
#' @examples
#' \dontrun{
#' calcOutput("MapBiomas", subtype="SecVeg")
#' }

calcMapBiomas <- function(subtype = "SecVeg") {

  # Read raw MapBiomas data
  mag <- readSource("MapBiomas", subtype = subtype)

  # Aggregate secondary vegetation classes
  mag <- dimSums(mag,dim = "variable")
  names(dimnames(mag))[3] <- "variable"
  getItems(mag, dim = "variable") <- "SecVeg"

  # Keep only selected years
  years <- c(1995, 2000, 2005, 2010, 2015, 2020)
  mag <- mag[, paste0("y", years), , drop = FALSE]

  # Convert from ha to Mha
  mag <- mag / 1e6

  return(mag)
}