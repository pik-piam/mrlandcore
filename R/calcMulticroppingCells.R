#' @title calcMulticroppingCells
#'
#' @description Returns grid cells and crops where multiple cropping takes place given the
#'              chosen scenario
#'
#' @param selectyears   Years to be returned
#' @param lpjml         LPJmL version required for respective inputs: natveg or crop
#' @param climatetype   Switch between different climate scenarios or
#'                      historical baseline "GSWP3-W5E5:historical"
#' @param scenario      "actual:total": currently multicropped areas calculated from total harvested areas
#'                                      and total physical areas per cell from readLandInG
#'                      "actual:crop" (crop-specific), "actual:irrigation" (irrigation-specific),
#'                      "actual:irrig_crop" (crop- and irrigation-specific) "total"
#'                      "potential:endogenous": potentially multicropped areas given
#'                                              temperature and productivity limits
#'                      "potential:exogenous": potentially multicropped areas given
#'                                             GAEZ suitability classification
#' @param sectoral      "kcr" MAgPIE crops, and "lpj" LPJmL crops
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("MulticroppingCells", aggregate = FALSE)
#' }
#'
#' @importFrom madrat calcOutput readSource toolGetMapping
#' @importFrom magclass dimSums dimOrder
#'

calcMulticroppingCells <- function(selectyears, lpjml, climatetype, scenario, sectoral = "kcr") {
  # extract sub-scenario
  subscenario <- strsplit(scenario, split = ":")[[1]][2]
  scenario    <- strsplit(scenario, split = ":")[[1]][1]

  if (grepl(pattern = "potential", x = scenario)) {
    # Cells that can potentially be multi-cropped (irrigation- and crop-specific)
    mcCells <- calcOutput("MulticroppingSuitability", selectyears = selectyears,
                          lpjml = lpjml, climatetype = climatetype,
                          suitability = subscenario, sectoral = sectoral,
                          aggregate = FALSE)

  } else if (grepl(pattern = "actual", x = scenario)) {
    # Cropping Intensity Factor (between 1 and 2)
    currMC <- calcOutput("MulticroppingIntensity", scenario = subscenario,
                         sectoral = sectoral,
                         selectyears = selectyears, aggregate = FALSE)

    mcCells         <- currMC
    mcCells[, , ]   <- 0
    mcCells[currMC > (1 + 1e-3)] <- 1    ## @Jens: please double-check

  } else {
    stop("Chosen scenario in calcMulticroppingCells not available:
         Please select potential for grid cells that can be potentially multi-cropped,
         or actual for gird cells that are currently multi-cropped,
         and sub-specification (for actual: total, crop, irrig, cropIrrig,
         for potential: endogenous, exogenous) separated by :")
  }

  ##############
  ### Checks ###
  ##############

  if (any(is.na(mcCells))) {
    stop("calcMulticroppingCells produced NA values")
  }
  if (any(mcCells != 1 & mcCells != 0)) {
    stop("Problem in calcMulticroppingCells: Value should be 0 or 1!")
  }

  ##############
  ### Return ###
  ##############
  out         <- mcCells
  unit        <- "boolean"
  description <- paste0("Gridcell- and crop-specific multiple cropping ",
                        "under irrigated and rainfed conditions. ",
                        "1 = multi-cropped, 0 = not multi-cropped")

  return(list(x            = out,
              weight       = NULL,
              unit         = unit,
              description  = description,
              isocountries = FALSE))
}
