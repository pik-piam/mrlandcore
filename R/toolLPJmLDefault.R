#' @title toolLPJmLDefault
#'
#' @description Specify default settings for LPJmL version, climate and baseline settings
#'
#' @param suppressNote suppress note (if TRUE)
#' @return configuration as list
#' @author Kristine Karstens
#'
#' @param suppressNote Suppress Notes
#' @importFrom stringr str_split
#'
#' @export

toolLPJmLDefault <- function(suppressNote = FALSE) {

  cfg <- NULL

  ##### DEFAULT CONFIG #####

  ### Harmonization settings
  cfg$baselineHist         <- "GSWP3-W5E5:historical"
  cfg$refYearHist          <- "y2010"
  cfg$baselineGcm          <- "MRI-ESM2-0:ssp245"
  cfg$refYearGcm           <- "y2025"

  ### Default version settings
  #   Use only for purely historical purposes
  cfg$defaultLPJmLVersion  <- "lpjml5.10.0-m4"
  #   Use for climate inputs future and past
  cfg$climateInputScen     <- "ISIMIP3bv2"
  cfg$climateInputHist     <- "ISIMIP3av2"

  ##### DEFAULT CONFIG #####

  if (!suppressNote) {
    vcat(2, paste0("Item defaultLPJmLVersion (", cfg$defaultLPJmLVersion, ") used in toolLPJmLDefault, ",
                   "might differ from the LPJmL version setting in preprocessing config."))
  }

  return(cfg)
}
