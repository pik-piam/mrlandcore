#' @title toolLPJmLScenario
#'
#' @description Specify default settings for LPJmL version and baseline settings
#'
#' @param lpjmlversion Switch between LPJmL versions (including add-ons (+*) for further version specification)
#' @param climatetype  Switch between different climate scenarios
#' @param subtype      Switch between runtype and variables in LPJmL
#'
#' @return configuration as list
#' @author Kristine Karstens
#'
#' @importFrom stringr str_split
#'
#' @export

toolLPJmLScenario <- function(lpjmlversion, climatetype, subtype) {

  cfg <- NULL

  ##### DEFAULT CONFIG #####
  cfg$version     <- lpjmlversion
  cfg$climatetype <- climatetype
  cfg$subtype     <- subtype
  ##### DEFAULT CONFIG #####

  ##### ADDON CONFIG #####
  # overwrite default runtype settings and LPJmL version for
  # (1) add-on tag in version argument - implemented add-ons:
  #  * `+scen_<scenname>(_<runtype>)`  - change runtype setting

  if (grepl("\\+scen", lpjmlversion)) {

    scen         <- unlist(strsplit(lpjmlversion, split = "\\+"))[2]
    cfg$version  <- unlist(strsplit(lpjmlversion, split = "\\+"))[1]
    scenParts    <- unlist(strsplit(scen, split = "_"))
    subtypeParts <- toolSplitSubtype(subtype, list(runtype = NULL, variable = NULL))

    if (!is.na(scenParts[3])) {
      # If scenario settings is specified for a specific runtype
      # only change runtype to addon-runtype for the specified runtype
      if (grepl(scenParts[3], subtypeParts$runtype)) {
        cfg$subtype  <- paste0(subtypeParts$runtype, "_", scenParts[2], ":", subtypeParts$variable)
      }
      # else subtype can stay as default

    } else {
      # If scenario settings is not specified for a specific runtype
      # change runtype to addon-runtype in any case
      cfg$subtype  <- paste0(subtypeParts$runtype, "_", scenParts[2], ":", subtypeParts$variable)
    }
  }
  ##### ADDON CONFIG #####

  return(cfg)
}
