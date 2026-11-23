#' @title calcNPPyearly
#'
#' @description This function extracts NPP from LPJmL and transforms it to
#'              yearly values including unit transformations
#'
#' @param subtype       Type of NPP to be returned.
#'                      Options:
#'                      "preind": reference pre-industrial NPP from pi-control run
#'                      "pnv": potetnial NPP from potential natural vegetation run
#' @param lpjml         Defines LPJmL version for main crop inputs
#' @param climatetype   Switch between different climate scenarios
#' @param unit          Unit to be returned.
#'                      Options:
#'                      "tC/m2" (tons carbon per square meter, original LPJmL unit)
#'                      "tC/ha" (tons carbon per hectare)
#'                      "tDM/ha" (tons dry matter per hectare, standard MAgPIE unit)
#'
#' @return magpie object in cellular resolution
#'
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("NPPyearly", aggregate = FALSE)
#' }
#'
#' @export

calcNPPyearly <- function(subtype = "preind",
                          lpjml = "lpjml5.10.0-m4",
                          climatetype = "MRI-ESM2-0:ssp245",
                          unit = "tC/m2") {

  if (subtype == "preind") {
    # Pre-industrial NPP (from picontrol run, transformed to tDM/ha)
    npp <- calcOutput("LPJmLTransform", lpjmlversion = lpjml,
                      climatetype = "GSWP3-W5E5:picontrol",
                      subtype     = "pnv:npp", subdata = NULL,
                      stage       = "raw",
                      monthly     = "FALSE:sum",
                      aggregate   = FALSE)
    description <- "Average pre-industrial NPP from pi-control run from LPJmL"

    # Average over entire pre-industrial period
    npp <- dimSums(npp, dim = "year") / length(getItems(npp, dim = "year"))

  } else if (subtype == "pnv") {
    # Smoothed and harmonized time series of NPP for potential natural vegetation
    npp <- calcOutput("LPJmLHarmonize", lpjmlversion =  lpjml,
                      climatetype = climatetype,
                      subtype     = "pnv:npp", subdata = NULL,
                      monthly     = "FALSE:sum",
                      aggregate   = FALSE)
    description <- "Potential NPP from potential natural vegetation run from LPJmL"
  } else {
    stop("Chosen type is not available yet. Please implement needed type of NPP
         in calcNPPyearly.")
  }

  ###########################
  ### Unit transformation ###
  ###########################
  if (unit == "tC/m2") {
    # transform from tDM/ha to tC/m2
    npp <- npp * 0.45 * 1e-4
  } else if (unit == "tC/ha") {
    # transform from tDM/ha to tC/ha
    npp <- npp * 0.45
  } else if (unit == "tDM/ha") {
    # no transformation needed
    # this unit is returned by calcLPJmLTransform/Harmonize
  } else {
    stop("Please choose unit in which NPP should be returned.
         Options: `tC/m2` or `tDM/ha`.")
  }

  #############################
  ### Weight for aggregation ##
  #############################
  # Total land area according to LPJmL in iniyear (constant over time and scenarios)
  landarea <- setYears(calcOutput("LPJmLTransform", lpjmlversion = lpjml,
                                  climatetype = "GSWP3-W5E5:historical",
                                  subtype     = "pnv:land_area", subdata = NULL,
                                  stage       = "raw",
                                  monthly     = FALSE,
                                  aggregate   = FALSE)[, "y1995", ], NULL)

  return(list(x            = npp,
              weight       = landarea,
              unit         = paste(unit, "/yr"),
              description  = description,
              isocountries = FALSE))
}
