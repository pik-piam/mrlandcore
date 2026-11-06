#' @title calcLPJmLHarmonize
#' @description Handle harmonization of LPJmL data
#'
#' @param lpjmlversion Switch between LPJmL versions (including addons for further version specification)
#' @param climatetype  Switch between different climate scenarios
#' @param subtype      Switch between different LPJmL output variables as specified in readLPJmL
#' @param subdata      Selection of subitems of object.
#'                     This argument can be used to split up the data in smaller objects
#'                     where only a sub-set of the data is needed or
#'                     for better handling where otherwise memory issues would occur due to the object size.
#' @param monthly      Boolean indicating whether monthly data (if available)
#'                     should be returned (TRUE) or whether it should be aggregated to yearly values (FALSE).
#'                     If FALSE: Need to indicate aggregation method, e.g.,
#'                     "sum" (for fluxes), "mean" (for stocks/pools);
#'                     Argument should be separated by ":" (e.g., "FALSE:sum")
#'
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#'
#' @author Kristine Karstens, Felicitas Beier
#'
#' @importFrom madrat calcOutput readSource toolSubtypeSelect toolSplitSubtype
#' @importFrom magclass dimSums getYears setYears
#'
#' @seealso
#' [toolLPJmLHarmonize()]
#' @examples
#' \dontrun{
#' calcOutput("LPJmLHarmonize", subtype = "pnv:soilc", aggregate = FALSE)
#' }
#' @export
#'

calcLPJmLHarmonize <- function(lpjmlversion = "lpjml5.10.0-m4",
                               climatetype = "MRI-ESM2-0:ssp245",
                               subtype = "pnv:soilc", subdata = NULL,
                               monthly     = FALSE) {

  # Extract settings for LPJmL from version and climatetype argument
  cfg <- toolLPJmLHarmonize(lpjmlversion = lpjmlversion, climatetype = climatetype)

  if (grepl("historical", climatetype)) {
    # return smoothed LPJmL data for historical baseline chosen
    x <- calcOutput("LPJmLTransform", lpjmlversion = cfg$readinVersion,
                    climatetype = cfg$climatetype, subtype = subtype,
                    subdata = subdata, stage = "smoothed:cut",
                    monthly = monthly,
                    aggregate = FALSE, supplementary = TRUE)
    # extract unit from LPJmL data
    unit <- x$unit
    # extract data
    out <- x$x
  } else {
    # The harmonization consists of two steps:
    # (1) the reference scenario is harmonized to reference baseline
    # based on the default settings of toolLPJmLHarmonization

    # read in historical data for subtype
    baseline <- calcOutput("LPJmLTransform", lpjmlversion = cfg$readinVersion,
                           climatetype = cfg$baselineHist, subtype = subtype,
                           subdata = subdata, stage = "smoothed:cut",
                           monthly = monthly,
                           aggregate = FALSE, supplementary = TRUE)
    unit     <- baseline$unit
    baseline <- baseline$x

    # read in future scenario data for subtype
    x <- calcOutput("LPJmLTransform", lpjmlversion = cfg$readinVersion,
                    climatetype = cfg$baselineGcm, subtype = subtype,
                    subdata = subdata, stage = "smoothed:cut",
                    monthly = monthly, aggregate = FALSE)

    # harmonize default future scenario to default baseline until
    # default historical year
    harmonizedScen <- toolHarmonize2Baseline(x, baseline, ref_year = cfg$refYearHist)

    # (2) the chosen climate scenario is harmonized to the reference scenario
    if (cfg$climatetype == cfg$baselineGcm) {
      # If climatetype identical to the default climatetype
      # from toolLPJmLHarmonize no additional harmonization is required
      out <- harmonizedScen
    } else {
      # Otherwise the scenario from the chosen climatetype is harmonized
      # to the default climatescenario for the reference year where the
      # GCM future starts

      # read in future scenario data for subtype
      x <- calcOutput("LPJmLTransform", lpjmlversion = cfg$readinVersion,
                      climatetype = cfg$climatetype, subtype = subtype,
                      subdata = subdata, stage = "smoothed:cut",
                      monthly = monthly, aggregate = FALSE)
      # harmonize chosen climate scenario to default baseline scenario
      out <- toolHarmonize2Baseline(x, harmonizedScen, ref_year = cfg$refYearGcm)
    }
  }

  # Define minimum threshold for returned data
  if (grepl("npp", subtype)) {
    # For NPP negative values are plausible, so no minimum value is set
    minThreshold <- NULL
  } else {
    # For all other data, the minimum threshold is set to 0 to
    # detect potential data inconsistencies
    minThreshold <- 0
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = unit,
              min          = minThreshold,
              description  = paste0("Output from LPJmL (", subtype, ") for ",
                                    lpjmlversion, " and ", climatetype, "."),
              isocountries = FALSE))
}
