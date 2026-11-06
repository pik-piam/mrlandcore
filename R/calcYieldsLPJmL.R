#' @title calcYieldsLPJmL
#'
#' @description This function extracts yields from LPJmL in the first season (main growing period)
#'              and calculates the yields for the off season (second growing period)
#'
#' @param lpjml         Defines LPJmL version for main crop inputs
#' @param climatetype   Switch between different climate scenarios
#' @param selectyears   Option to reduce number of years to be returned
#'                      (e.g., to avoid memory issues)
#' @param multicropping Multicropping activated (TRUE) or not (FALSE) and
#'                      Multiple Cropping Suitability mask selected
#'                      (mask can be:
#'                      "none": no mask applied (only for development purposes)
#'                      "actual:total": currently multicropped areas calculated from total harvested areas
#'                                      and total physical areas per cell from readLandInG
#'                      "actual:crop" (crop-specific), "actual:irrigation" (irrigation-specific),
#'                      "actual:irrig_crop" (crop- and irrigation-specific),
#'                      "potential:endogenous": potentially multicropped areas given
#'                                              growing conditions from LPJmL
#'                      "potential:exogenous": potentially multicropped areas given
#'                                             GAEZ suitability classification)
#'                      (e.g. TRUE:actual:total; TRUE:none; FALSE)
#'
#' @return magpie object in cellular resolution
#'
#' @author Felicitas Beier, Kristine Karstens
#'
#' @examples
#' \dontrun{
#' calcOutput("YieldsLPJmL", aggregate = FALSE)
#' }
#'
#' @importFrom madrat toolGetMapping
#' @importFrom withr local_options
#' @importFrom stringr str_split
#' @importFrom stats quantile

calcYieldsLPJmL <- function(lpjml = "lpjml5.10.0-m4",
                            climatetype = "MRI-ESM2-0:ssp245",
                            selectyears = seq(1965, 2100, by = 5),
                            multicropping = FALSE) {

  # Extract multiple cropping argument information
  if (as.logical(stringr::str_split(multicropping, ":")[[1]][1])) {
    areaMask <- paste(unlist(strsplit(multicropping, ":"))[2],
                      unlist(strsplit(multicropping, ":"))[3],
                      sep = ":")
    multicropping <- as.logical(unlist(strsplit(multicropping, ":"))[1])
  }

  # Increase object size limit
  withr::local_options(magclass_sizeLimit = 1e+12)

  # LPJmL crop types
  lpj2mag     <- toolGetMapping("MAgPIE_LPJmL.csv", type = "sectoral", where = "mrlandcore")
  cropsLPJmL  <- unique(lpj2mag$LPJmL5)

  # Combine irrigated and rainfed yields for all crops
  irYlds <- list()
  rfYlds <- list()
  yields <- list()
  # Read in by crop because of memory issues
  for (crop in cropsLPJmL) {
    # irrigated yields in irrigated growing period (in tDM/ha)
    irYlds[[crop]] <- calcOutput("LPJmLHarmonize",
                                 subtype = "cropsIR:pft_harvestc",
                                 subdata = paste("irrigated", crop, sep = "."),
                                 lpjmlversion = lpjml, climatetype = climatetype,
                                 years = selectyears, monthly = FALSE,
                                 aggregate = FALSE)

    if (crop == "grassland") {
      t <- dimSums(calcOutput("LPJmLHarmonize",
                              subtype = "grass:gpp",
                              lpjmlversion = lpjml,
                              climatetype = climatetype,
                              years = selectyears, monthly = FALSE,
                              aggregate = FALSE),
                   dim = 3) * 0.23 # HACKATHON: Document magical number from Jens
      t <- add_dimension(t, dim = 3.1, add = "irrigation", nm = "rainfed")
      t <- add_dimension(t, dim = 3.2, add = "crop",       nm = "grassland")
      rfYlds[[crop]] <- t
    } else {
      rfYlds[[crop]] <- calcOutput("LPJmLHarmonize",
                                   subtype = "cropsRF:pft_harvestc",
                                   subdata = paste("rainfed", crop, sep = "."),
                                   lpjmlversion = lpjml, climatetype = climatetype,
                                   years = selectyears, monthly = FALSE,
                                   aggregate = FALSE)
    }

    # irrigated and rainfed yields in main growing period (in tDM/ha)
    yields[[crop]] <- mbind(rfYlds[[crop]], irYlds[[crop]])
  }
  yields <- mbind(yields)
  yields <- dimOrder(yields, perm = c(2, 1), dim = 3)

  # For case of multiple cropping, off-season yield needs to be calculated
  if (multicropping) {
    # Multiple cropping yield increase factor
    increaseFactor <- calcOutput("MulticroppingYieldIncrease",
                                 lpjml = lpjml,
                                 climatetype = climatetype,
                                 selectyears = selectyears,
                                 aggregate = FALSE)[, , getItems(yields, dim = 3)]

    # Main-season yield
    mainYield <- yields
    # Off-season yield
    offYield  <- yields * increaseFactor

    # Cap for off-season yield due to numerical reasons
    ## To Do: speed up this part of the function, e.g. using apply
    for (y in getItems(offYield, dim = 2)) {
      for (k in getItems(offYield, dim = 3)) {
        cap <- quantile(offYield[, y, k], 0.999, na.rm = TRUE)
        offYield[, y, k][offYield[, y, k] > cap] <- cap
      }
    }

    # Off-season yield minimum (small off season yields are excluded)
    # The assumption is that no multiple cropping takes place where the additional
    # yield that can be achieved in the second season is very small
    offYield[offYield < 0.5] <- 0

    # Multiple cropping share
    if (areaMask == "none") {
      mcShr <- calcOutput("MulticroppingCells",
                          sectoral = "lpj",
                          scenario = "potential:exogenous",
                          lpjml = lpjml,
                          climatetype = climatetype,
                          selectyears = selectyears,
                          aggregate = FALSE)
      # multiple cropping is allowed everywhere
      mcShr[, , ] <- 1
    } else if (grepl(pattern = "actual", x = areaMask)) {
      # Cropping Intensity Factor (between 1 and 2)
      ci <- calcOutput("MulticroppingIntensity", scenario = strsplit(areaMask, split = ":")[[1]][2],
                       sectoral = "lpj",
                       selectyears = selectyears, aggregate = FALSE)
      # Share that is multiple cropped
      mcShr <- ci - 1
    } else {
      # for potential case: cell is fully multiple cropped
      mcShr <- calcOutput("MulticroppingCells", scenario = areaMask,
                          sectoral = "lpj",
                          lpjml = lpjml,
                          climatetype = climatetype,
                          selectyears = selectyears,
                          aggregate = FALSE)
    }
    # Add grassland to mcShr object with suitability set to 0
    # Note: The grassland growing period is already the whole year, so no multiple
    #       cropping treatment necessary.
    mcShr <- add_columns(mcShr, dim = "crop", addnm = "grassland", fill = 0)
    mcShr <- mcShr[, , getItems(yields, dim = 3)]

    # Whole year yields under multicropping (main-season yield + off-season yield)
    yields <- mainYield + offYield * mcShr

  } else {
    # Only main season yields are returned
    yields  <- yields
  }

  # Check for NA's and negative yields
  if (any(is.na(yields))) {
    stop("calcYieldsLPJmL produced NA yields")
  }
  if (any(yields < -1e-10)) {
    stop("calcYieldsLPJmL produced negative yields")
  }
  yields[yields < 0] <- 0

  return(list(x            = yields,
              weight       = NULL,         ### Kristine, Jan: Should we provide a weight here?
              unit         = "tDM per ha",
              description  = "Yields for LPJmL crop types.",
              isocountries = FALSE))
}
