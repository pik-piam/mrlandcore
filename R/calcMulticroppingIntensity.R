#' @title calcMulticroppingIntensity
#'
#' @description Returns cropping intensity according to LandInG data
#'              given the chosen scenario
#'
#' @param selectyears   Years to be returned
#' @param scenario      "total": currently multicropped areas calculated from total harvested areas
#'                               and total physical areas per cell from readLandInG
#'                      "crop" (crop-specific), "irrigation" (irrigation-specific),
#'                      "irrig_crop" (crop- and irrigation-specific)
#' @param sectoral      "kcr" MAgPIE crop types, and "lpj" LPJmL crop types
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("MulticroppingIntensity", aggregate = FALSE)
#' }
#'
#' @importFrom madrat calcOutput readSource toolGetMapping
#' @importFrom magclass dimSums
#'

calcMulticroppingIntensity <- function(scenario, selectyears,
                                       sectoral = "lpj") {

  if (grepl("kcr", sectoral)) {

    # Crops that are not suitable for multiple cropping (either because they are a perennial
    # crop that's grown over several years or because the growing period is too long to
    # allow for another season
    cropsNoMC <- c("sugr_cane", "oilpalm", "betr", "begr")

  } else if (grepl("lpj", sectoral)) {

    # Crops that are not suitable for multiple cropping (either because they are a perennial
    # crop that's grown over several years or because the growing period is too long to
    # allow for another season
    cropsNoMC <- c("sugarcane", "biomass tree", "biomass grass")

  }

  # Croparea (LandInG) only covers the historical period. Requested years beyond
  # the last available year are filled with the values of that year, holding the cropping
  # intensity constant afterwards
  phys <- calcOutput("Croparea", physical = TRUE, fallow = FALSE,
                     sectoral = sectoral,
                     cellular = TRUE, irrigation = TRUE,
                     aggregate = FALSE)
  # toolHoldConstant fills every missing year, including ones before the data start, with
  # the value of the last year, so this case has to be excluded before extending the years
  if (min(as.integer(gsub("y", "", selectyears))) < min(getYears(phys, as.integer = TRUE))) {
    stop("In calcMulticroppingIntensity: selected years are before the first available ",
         "year of Croparea (LandInG) and cannot be filled with a held constant value.")
  }
  phys <- toolHoldConstant(phys, years = selectyears)[, selectyears, ]
  harv <- toolHoldConstant(calcOutput("Croparea", physical = FALSE, fallow = FALSE,
                                      sectoral = sectoral,
                                      cellular = TRUE, irrigation = TRUE,
                                      aggregate = FALSE),
                           years = selectyears)[, selectyears, ]
  # keep for dimensionality
  phys[, , ] <- NA
  harv[, , ] <- NA

  if (scenario == "total") {
    # total actual multicropping area
    tempPhys <- dimSums(toolHoldConstant(calcOutput("Croparea", physical = TRUE, fallow = FALSE,
                                                    sectoral = sectoral,
                                                    cellular = TRUE, irrigation = FALSE,
                                                    aggregate = FALSE),
                                         years = selectyears)[, selectyears, ],
                        dim = "crop")
    tempHarv <- dimSums(toolHoldConstant(calcOutput("Croparea", physical = FALSE, fallow = FALSE,
                                                    sectoral = sectoral,
                                                    cellular = TRUE, irrigation = FALSE,
                                                    aggregate = FALSE),
                                         years = selectyears)[, selectyears, ],
                        dim = "crop")

    # expand dimension
    phys[, , ] <- tempPhys
    harv[, , ] <- tempHarv

  } else if (scenario == "irrig") {
    # total actual multicropping area (irrigation-specific)
    tempPhys <- dimSums(toolHoldConstant(calcOutput("Croparea", physical = TRUE, fallow = FALSE,
                                                    sectoral = sectoral,
                                                    cellular = TRUE, irrigation = TRUE,
                                                    aggregate = FALSE),
                                         years = selectyears)[, selectyears, ],
                        dim = "crop")
    tempHarv <- dimSums(toolHoldConstant(calcOutput("Croparea", physical = FALSE, fallow = FALSE,
                                                    sectoral = sectoral,
                                                    cellular = TRUE, irrigation = TRUE,
                                                    aggregate = FALSE),
                                         years = selectyears)[, selectyears, ],
                        dim = "crop")

    # expand dimension
    phys[, , ] <- tempPhys
    harv[, , ] <- tempHarv

  } else if (scenario == "crop") {
    # areas where multicropping takes place currently (crop-specific)
    tempPhys <- toolHoldConstant(calcOutput("Croparea", physical = TRUE, fallow = FALSE,
                                            sectoral = sectoral,
                                            cellular = TRUE, irrigation = FALSE,
                                            aggregate = FALSE),
                                 years = selectyears)[, selectyears, ]
    tempHarv <- toolHoldConstant(calcOutput("Croparea", physical = FALSE, fallow = FALSE,
                                            sectoral = sectoral,
                                            cellular = TRUE, irrigation = FALSE,
                                            aggregate = FALSE),
                                 years = selectyears)[, selectyears, ]

    # expand dimension
    phys[, , ] <- tempPhys
    harv[, , ] <- tempHarv

  } else if (scenario == "irrig_crop") {
    # areas where multiple cropping takes place currently (crop- and irrigation-specific)
    phys <- toolHoldConstant(calcOutput("Croparea", physical = TRUE, fallow = FALSE,
                                        sectoral = sectoral,
                                        cellular = TRUE, irrigation = TRUE,
                                        aggregate = FALSE),
                             years = selectyears)[, selectyears, ]
    harv <- toolHoldConstant(calcOutput("Croparea", physical = FALSE, fallow = FALSE,
                                        sectoral = sectoral,
                                        cellular = TRUE, irrigation = TRUE,
                                        aggregate = FALSE),
                             years = selectyears)[, selectyears, ]

  } else {
    stop("Please select whether total, irrigation-specific (irrig), crop-specific (crop),
    or irrigation- and crop-specific (irrig_crop) cropping intensity shall be
    returned in the scenario argument of calcMulticroppingIntensity")
  }

  # cropping intensity factor
  currMC <- ifelse(phys > 0, harv / phys, NA)

  # corrections
  currMC[currMC > 2]    <- 2
  currMC[is.na(currMC)] <- 1

  # change dimension order of third dimension
  currMC <- dimOrder(currMC, c(2, 1), dim = 3)

  ###################
  ### Corrections ###
  ###################
  # round share to avoid numerical problems
  currMC <- round(currMC, digits = 2)

  ##############
  ### Checks ###
  ##############
  if (any(is.na(currMC))) {
    stop("mrland::calcMulticroppingIntensity produced NA values")
  }
  if (any(currMC > 2 | currMC < 1)) {
    stop("Problem in mrland::calcMulticroppingIntensity:
         Value should be between 1 and 2!")
  }
  if (any(currMC[, , cropsNoMC] > 1)) {
    stop(paste0("Problem in mrland::calcMulticroppingIntensity: ",
                "Crops that are not suitable for multiple cropping ",
                "should not have CI > 1"))
  }

  ##############
  ### Return ###
  ##############
  out         <- currMC
  unit        <- "factor"
  description <- paste0("Cropping Intensitiy of different crops ",
                        "under irrigated and rainfed conditions respectively")

  return(list(x            = out,
              weight       = phys,
              aggregationArguments = list(zeroWeight = "fix"),
              unit         = unit,
              description  = description,
              isocountries = FALSE))
}
