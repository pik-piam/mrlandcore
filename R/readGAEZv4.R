#' @title readGAEZv4
#' @description Read in data from the Global Agro-ecological Zones (GAEZ) data set version 4
#' @param subtype Name of GAEZ file to be read
#'                Available are for example:
#'                "mci_CRUTS32_Hist_0010.tif" (irrigated multiple cropping suitability for years 2000-2010),
#'                "mcr_CRUTS32_Hist_0010.tif" (rainfed multiple cropping suitability for years 2000-2010)
#' @return MAgPIE object at 0.5 cellular level
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' readSource("GAEZv4", convert = "onlycorrect")
#' }
#'
#' @importFrom magclass as.magpie mbind getNames
#' @importFrom raster brick raster projectRaster

readGAEZv4 <- function(subtype = "mci_CRUTS32_Hist_0010.tif") {

  # Transform from 0.08 to 0.5 spatial resolution and convert to magpie object
  .transformObject <- function(x) {
    x <- brick(projectRaster(from = x, to = raster(resolution = 0.5), method = "ngb", over = TRUE))
    x <- as.magpie(x)
    return(x)
  }

  # read data
  x <- brick(subtype)
  # transform data
  x <- .transformObject(x = x)

  if (any(is.na(x))) {
    stop("produced NA multiple cropping zones")
  }

  return(x)
}
