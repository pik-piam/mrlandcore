#' @title toolClimateInputVersion
#'
#' @description Specify default settings for LPJmL climate input version and baseline settings
#' @param lpjmlVersion Add-ons (+*) for further version specification for LPJmL version
#' @param climatetype Switch between different climate scenarios
#'
#' @return configuration as list
#' @author Kristine Karstens
#'
#' @importFrom stringr str_split
#'
#' @export

toolClimateInputVersion <- function(lpjmlVersion, climatetype) {

  cfgLPJmL <- toolLPJmLDefault(suppressNote = TRUE)
  cfg      <- NULL

  ##### DEFAULT CLIMATE CONFIG #####
  cfg$versionScen   <- cfgLPJmL$climateInputScen
  cfg$versionHist   <- cfgLPJmL$climateInputHist
  cfg$baselineHist  <- cfgLPJmL$baselineHist
  cfg$refYearHist   <- cfgLPJmL$refYearHist
  cfg$baselineGcm   <- cfgLPJmL$baselineGcm
  cfg$refYearGcm    <- cfgLPJmL$refYearGcm
  cfg$climatetype   <- climatetype
  ##### DEFAULT  CLIMATE CONFIG #####

  if (cfg$climatetype == "GSWP3-W5E5:historical") {
    cfg$climatetype    <- "GSWP3-W5E5:obsclim"
  }

  return(cfg)
}
