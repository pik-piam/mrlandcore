#' @title calcRicearea
#' @description calculates rice area based on LUH flooded areas and
#'              physical rice areas reported by FAOSTAT.
#'
#' @param cellular   If TRUE: calculates cellular rice area
#' @param share      If TRUE: non-flooded share is returned.
#'                   If FALSE: rice area (flooded, non-flooded, total) in Mha is returned
#'
#' @return rice areas or rice area shares of flooded and non-flooded category
#'
#' @author Felicitas Beier, Kristine Karstens
#'
#' @importFrom magpiesets findset
#' @importFrom withr local_options

calcRicearea <- function(cellular = FALSE, share = TRUE) {

  local_options(magclass_sizeLimit = 1e+12)

  ############################################
  ### Ricearea and shares on country level ###
  ############################################

  # Country-level LUH flooded areas
  floodedLUHiso  <- collapseNames(calcOutput("LUH2v2", landuse_types = "flooded",
                                             aggregate = FALSE, irrigation = TRUE,
                                             selectyears = seq(1965, 2015, 5),
                                             cellular = FALSE))

  # FAO rice areas (physical to be comparable with LUH)
  riceareaFAOiso <- collapseNames(calcOutput("Croparea", sectoral = "kcr", physical = TRUE,
                                             cellular = FALSE, irrigation = FALSE,
                                             aggregate = FALSE)[, , "rice_pro"])

  commonYears     <- intersect(getYears(riceareaFAOiso), getYears(floodedLUHiso))
  floodedLUHiso   <- floodedLUHiso[, commonYears, ]
  riceareaFAOiso  <- riceareaFAOiso[, commonYears, ]

  # Country-level rice area
  ricearea <- floodedLUHiso

  # Correction for flooded non-rice areas (floodedLUHiso > riceareaFAOiso)
  ricearea[floodedLUHiso > riceareaFAOiso] <- riceareaFAOiso[floodedLUHiso > riceareaFAOiso]
  nonriceShr                               <- ifelse(floodedLUHiso > 0,
                                                     ricearea / floodedLUHiso,
                                                     0)

  # Correction for aerobic (non-paddy) rice (floodedLUHiso < riceareaFAOiso)
  floodedRicearea                                 <- riceareaFAOiso
  floodedRicearea[floodedLUHiso < riceareaFAOiso] <- floodedLUHiso[floodedLUHiso < riceareaFAOiso]
  floodedShr                                      <- ifelse(riceareaFAOiso > 0,
                                                            floodedRicearea / riceareaFAOiso,
                                                            0)

  # Non-flooded rice area
  nonfloodedRicearea <- ricearea * (1 - floodedShr)

  if (!cellular) {

    if (share) {

      out         <- 1 - floodedShr
      unit        <- "Share"
      description <- "Share of rice area that is non-flooded"

    } else {

      ricearea           <- add_dimension(ricearea, dim = 3.1,
                                          add = "type", nm = "total")
      floodedRicearea    <- add_dimension(floodedRicearea, dim = 3.1,
                                          add = "type", nm = "flooded")
      nonfloodedRicearea <- add_dimension(nonfloodedRicearea, dim = 3.1,
                                          add = "type", nm = "nonflooded")

      out         <- collapseNames(mbind(ricearea, floodedRicearea, nonfloodedRicearea))
      unit        <- "Mha"
      description <- "Physical rice area on country level"
    }

  } else {
    ############################################
    ### Ricearea and shares on cellular level
    ############################################

    # Cellular LUH flooded areas
    floodedLUH <- collapseNames(calcOutput("LUH2v2", landuse_types = "flooded", cellular = TRUE, irrigation = TRUE,
                                           selectyears = seq(1965, 2015, 5), aggregate = FALSE))

    commonYears <- intersect(getYears(nonriceShr), getYears(floodedLUH))
    floodedLUH  <- floodedLUH[, commonYears, ]
    nonriceShr  <- nonriceShr[, commonYears, ]

    # Correction for flooded non-rice areas (floodedLUHiso > riceareaFAOiso)
    commonCountries <- intersect(getItems(nonriceShr, dim = "country"), getItems(floodedLUH, dim = "iso"))
    ricearea <- floodedLUH * nonriceShr[commonCountries, , ]

    # Correction for aerobic (non-paddy) rice (floodedLUHiso < riceareaFAOiso)
    floodedRicearea    <- ricearea * floodedShr[commonCountries, , ]
    nonfloodedRicearea <- ricearea * (1 - floodedShr)[commonCountries, , ]
    ricearea           <- floodedRicearea + nonfloodedRicearea

    ricearea           <- add_dimension(ricearea, dim = 3.1, add = "type", nm = "total")
    floodedRicearea    <- add_dimension(floodedRicearea, dim = 3.1, add = "type", nm = "flooded")
    nonfloodedRicearea <- add_dimension(nonfloodedRicearea, dim = 3.1, add = "type", nm = "nonflooded")

    out         <- collapseNames(mbind(floodedRicearea, nonfloodedRicearea, ricearea))
    unit        <- "Mha"
    description <- "Physical rice area on cellular level"

    if (share) {
      stop("Argument share = TRUE not supported with cellular = TRUE.
           Please select cellular = FALSE to return flooded rice area share")
    }
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = unit,
              description  = description,
              isocountries = !cellular))
}
