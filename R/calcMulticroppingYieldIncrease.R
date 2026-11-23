#' @title calcMulticroppingYieldIncrease
#'
#' @description Calculates yield increase achieved through multiple cropping
#'              (as factor of off season to main season crop yield) under
#'              irrigated and rainfed conditions respectively.
#'              Optionally: return which grid cells are potentially suitable for
#'              multiple cropping activities under rainfed and irrigated conditions.
#'              Calculation is based on grassland gross primary production (GPP)
#'              in the growing period of the respective crop and annual grass GPP.
#'
#' @param selectyears   Years to be returned
#' @param lpjml         LPJmL version required for respective inputs as single string: "crop" version
#' @param climatetype   Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param fallowFactor  Factor determining yield reduction in off season due to
#'                      fallow period between harvest of first (main) season and
#'                      sowing of second (off) season
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("MulticroppingYieldIncrease", aggregate = FALSE)
#' }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass getSets mbind getItems new.magpie
#'

calcMulticroppingYieldIncrease <- function(selectyears, lpjml, climatetype,
                                           fallowFactor = 0.75) {

  ####################
  ### Read in data ###
  ####################
  # grass GPP in the entire year (main + off season) where cropping is possible
  # taking the growing period months into account (in tDM/ha)
  grassGPPannual <- setYears(calcOutput("GrassGPPyearly", season = "wholeYear",
                                        lpjml = lpjml, climatetype = climatetype,
                                        selectyears = selectyears, aggregate = FALSE),
                             selectyears)
  # grass GPP in the growing period of LPJmL (main season) (in tDM/ha)
  grassGPPgrper  <- setYears(calcOutput("GrassGPPyearly", season = "mainSeason",
                                        lpjml = lpjml, climatetype = climatetype,
                                        selectyears = selectyears, aggregate = FALSE),
                             selectyears)

  # ensure that ordering of third dimension is the same for all objects
  cropIrrigList <- getItems(grassGPPannual, dim = 3)


  ####################
  ### Calculations ###
  ####################

  ### Yield Increase Factor  ###
  # Calculate multiple cropping factor based on annual grass GPP and
  # grass GPP in growing period of crop
  grassGPPoffseason <- (grassGPPannual[, , cropIrrigList] - grassGPPgrper[, , cropIrrigList])
  grassGPPoffseason[grassGPPoffseason < 0] <- 0

  increaseFACTOR <- ifelse(grassGPPgrper > 0,
                           grassGPPoffseason / grassGPPgrper,
                           0) * fallowFactor

  # Add missing crops (betr, begr, mgrass)
  # [Note: grown throughout the whole year] ---> set to 0
  missingCrops <- new.magpie(cells_and_regions = getItems(increaseFACTOR, dim = 1),
                             years = getItems(increaseFACTOR, dim = 2),
                             names = c("irrigated.biomass tree", "rainfed.biomass tree",
                                       "irrigated.biomass grass", "rainfed.biomass grass",
                                       "irrigated.grassland", "rainfed.grassland"),
                             fill = 0)
  getSets(missingCrops) <- getSets(increaseFACTOR)
  increaseFACTOR        <- mbind(increaseFACTOR, missingCrops)

  # Adjust dimension to same ordering as Yields functions
  increaseFACTOR <- dimOrder(increaseFACTOR, perm = c(2, 1), dim = 3)

  ##############
  ### Checks ###
  ##############

  if (any(is.na(increaseFACTOR))) {
    stop("calcMulticroppingYieldIncrease produced NA values")
  }
  if (any(increaseFACTOR < 0)) {
    stop("calcMulticroppingYieldIncrease produced negative values")
  }

  ##############
  ### Return ###
  ##############
  unit        <- "1"
  description <- paste0("Factor of yield increase through multiple cropping ",
                        "to be applied on LPJmL crop yield")

  return(list(x            = increaseFACTOR,
              weight       = NULL,
              unit         = unit,
              description  = description,
              isocountries = FALSE))
}
