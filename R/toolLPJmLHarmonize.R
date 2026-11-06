#' @title toolLPJmLHarmonize
#'
#' @description Specify default settings for LPJmL version and baseline settings
#'
#' @param lpjmlversion Switch between LPJmL versions (including add-ons (+*) for further version specification)
#' @param climatetype  Switch between different climate scenarios
#'
#' @return configuration as list
#' @author Kristine Karstens
#'
#' @importFrom stringr str_split
#'
#' @export

toolLPJmLHarmonize <- function(lpjmlversion, climatetype) {

  cfg <- NULL
  cfg <- toolLPJmLDefault(suppressNote = TRUE)
  # toolLPJmLDefault defines - see toolLPJmLDefault for detais
  ## cfg$baselineHist
  ## cfg$refYearHist
  ## cfg$baselineGcm
  ## cfg$refYearGcm
  ## cfg$defaultLPJmLVersion  - Use only for purely historical purposes
  ## cfg$climateInputScen     - Use for climate inputs future
  ## cfg$climateInputHist     - Use for climate inputs past

  cfg$readinVersion   <- lpjmlversion
  cfg$climatetype     <- climatetype

  ##### ADDON CONFIG #####
  # overwrite default settings and LPJmL version for
  # (1) add-on tag in version argument - implemented add-ons:
  #  * `+baselineGcm<GCM:RCP>`         - use another baseline for 2010--2020
  #  * `+scen_<scenname>(_<runtype>)` - implemented scenario will be handled toolLPJmLScenario

  ### version addon
  if (grepl("\\+", lpjmlversion)) {
    tmp <- unlist(str_split(lpjmlversion, "\\+"))

    if (any(grepl("baselineGcm", tmp))) {
      i <- grep("baselineGcm", tmp)
      cfg$baselineGcm   <- gsub("baselineGcm", "", tmp[i])
      cfg$readinVersion <- tmp[1]
    }
  }
  ##### ADDON CONFIG #####

  toolExpectTrue(cfg$readinVersion == cfg$defaultLPJmLVersion,
                 "defaultLPJmLVersion in toolLPJmLDefault identical to
                 LPJmLversion setting in preprocessing config.",
                 falseStatus = "note")

  return(cfg)
}
