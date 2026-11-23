#' @title calcGrassGPPyearly
#'
#' @description Calculates gross primary production (GPP) of grassland
#'              under irrigated and rainfed conditions based on LPJmL inputs.
#'
#' @param selectyears   Years to be returned
#' @param lpjml         LPJmL version required for respective inputs: natveg or crop
#' @param climatetype   Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param season        "wholeYear":  grass GPP in the entire year (main + off season)
#'                      "mainSeason": grass GPPP in the crop-specific growing
#'                                    period of LPJmL (main season)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{
#' calcOutput("GrassGPPyearly", aggregate = FALSE)
#' }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass dimSums getItems new.magpie getSets add_dimension
#'

calcGrassGPPyearly <- function(selectyears, lpjml, climatetype, season) {

  ####################
  ### Read in data ###
  ####################
  # Monthly grass GPP
  monthlyRainfed <- calcOutput("GrassGPPmonthly",
                               selectyears = selectyears,
                               lpjml = lpjml, climatetype = climatetype,
                               aggregate = FALSE)[, , "rainfed"]

  monthlyIrrigated <- calcOutput("GrassGPPmonthly",
                                 selectyears = selectyears,
                                 lpjml = lpjml, climatetype = climatetype,
                                 aggregate = FALSE)[, , "irrigated"]

  # irrigated grass GPP in irrigated growing period of crop (in tDM/ha)
  grperIrrigated <- calcOutput("LPJmLHarmonize", subtype = "cropsIR:cft_gpp_grass",
                               years = selectyears, monthly = FALSE,
                               lpjmlversion = lpjml, climatetype = climatetype,
                               aggregate = FALSE)[, , "irrigated"]

  # rainfed grass GPP in rainfed growing period of crop (in tDM/ha)
  grperRainfed <- calcOutput("LPJmLHarmonize", subtype = "cropsRF:cft_gpp_grass",
                             years = selectyears, monthly = FALSE,
                             lpjmlversion = lpjml, climatetype = climatetype,
                             aggregate = FALSE)[, , "rainfed"]

  ########################
  ### Data preparation ###
  ########################
  # Empty objects to be filled
  grassGPPannual <- grassGPPgrper <- new.magpie(cells_and_regions = getItems(grperIrrigated, dim = 1),
                                                years = getItems(grperIrrigated, dim = 2),
                                                names = c(getItems(grperIrrigated, dim = 3),
                                                          getItems(grperRainfed, dim = 3)),
                                                fill = NA)
  # Name dimensions
  getSets(grassGPPannual) <- c("x", "y", "iso", "year", "irrigation", "crop")
  getSets(grassGPPgrper)  <- c("x", "y", "iso", "year", "irrigation", "crop")

  # Extract rainfed grass GPP in rainfed growing period of crop
  grassGPPgrper[, , "rainfed"]   <- grperRainfed[, , "rainfed"]
  # Extract irrigated grass GPP in irrigated growing period of crop
  grassGPPgrper[, , "irrigated"] <- grperIrrigated[, , "irrigated"]

  ##############
  ### Return ###
  ##############
  unit        <- "tDM per ha"
  description <- "irrigated and rainfed gross primary production (GPP) of grass"

  if (season == "mainSeason") {

    out         <- grassGPPgrper
    description <- paste0(description, " in growing season of LPJmL")

  } else if (season == "wholeYear") {
    # read in months with favorable growing conditions (boolean: 1=growing month; 0=no growing month)
    grperPOT <- calcOutput("GrowingPeriodMonths",
                           selectyears = selectyears,
                           lpjml = lpjml, climatetype = climatetype,
                           aggregate = FALSE)

    # Calculate "annual" rainfed grass GPP for potential growing period
    # (i.e., months with favorable crop growth conditions)
    grassGPPannual[, , "rainfed"] <- dimSums(monthlyRainfed * grperPOT[, , "rainfed"],
                                             dim = 3)
    # Calculate "annual" irrigated grass GPP for potential growing period
    # (i.e., months with favorable crop growth conditions)
    grassGPPannual[, , "irrigated"] <- dimSums(monthlyIrrigated * grperPOT[, , "irrigated"],
                                               dim = 3)

    out         <- grassGPPannual
    description <- paste0(description, " in the entire year (when crop growth is possible)")

  } else {
    stop("Please specify output to be returned by function calcGrassGPPyearly:
         mainSeason or wholeYear")
  }

  ##############
  ### Checks ###
  ##############
  if (any(is.na(out))) {
    stop("calcGrassGPP produced NA values")
  }
  if (any(out < 0)) {
    stop("calcGrassGPP produced negative values")
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = unit,
              description  = description,
              isocountries = FALSE))
}
