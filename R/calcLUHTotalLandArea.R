#' @title calcLUHTotalLandArea
#'
#' @description calculates the total land area included in a LUH data set
#'
#' @param datasource Which LUH datasource should be used (\code{LUH2v2}, or \code{LUH3})
#' @return A magpie object including the LUH total land area per cell
#' @author Patrick Rein
#' @importFrom madrat calcOutput
#' @importFrom magclass setYears dimSums

calcLUHTotalLandArea <- function(datasource = "LUH3") {
  if (datasource == "LUH3") {
    area  <- calcOutput("LUH3", landuseTypes = "LUH3", irrigation = FALSE,
                        cellular = TRUE, yrs = 2010, aggregate = FALSE)
  } else if (datasource == "LUH2v2") {
    area  <- calcOutput("LUH2v2", landuse_types = "LUH2v2", irrigation = FALSE,
                        cellular = TRUE, selectyears = "2010", aggregate = FALSE)
  } else {
    stop(datasource, " is not a valid data source for calcLUHTotalLandArea")
  }
  area <- setYears(dimSums(area, dim = 3), NULL)
  return(list(x = area,
              isocountries = FALSE,
              description = paste0("The total land area in ", datasource),
              unit = "Mha"))
}