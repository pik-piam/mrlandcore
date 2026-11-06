#' @title calcLPJmLTransform
#' @description Transform LPJmL data in units and variables used by magpie
#'
#' @param lpjmlversion Switch between LPJmL versions (including addons for further version specification)
#' @param climatetype  Switch between different climate scenarios (climatemodel:climatescenario)
#' @param subtype      Switch between different lpjml input (runtype:filename)
#' @param subdata      Selection of subitems of object.
#'                     This argument can be used to split up the data in smaller objects
#'                     where only a sub-set of the data is needed or for better handling
#'                     where otherwise memory issues would occur due to the object size.
#' @param stage        Degree of processing: raw:cut, smoothed:cut           - raw or smoothed data from 1930|1951
#'                                           raw:fullhist, smoothed:fullhist - raw or smoothed data with full history
#' @param monthly      Controls handling of monthly LPJmL outputs.
#'                     The argument can be provided either as a logical value or as a character
#'                     string specifying an aggregation method.
#'                     TRUE: return monthly data (if available). Month indices are renamed from numeric values
#'                     to abbreviated month names.
#'                     FALSE: return the data without modification (default; appropriate for yearly inputs).
#'                     "FALSE:sum": aggregate monthly data to yearly values using the sum across months
#'                     (appropriate for fluxes, e.g. NPP/GPP/runoff/precipitation).
#'                     "FALSE:mean": aggregate monthly data to yearly values using the arithmetic mean across
#'                     months (appropriate for stocks/pools, e.g. soil carbon, biomass stocks).
#'                     If monthly = TRUE but no month dimension exists, an error is thrown.
#'
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#'
#' @author Kristine Karstens, Felicitas Beier
#'
#' @seealso
#' [readLPJmL()]
#' @examples
#' \dontrun{
#' calcOutput("LPJmLTransform", subtype = "pnv:soilc", aggregate = FALSE)
#' }
#' @export
#'
calcLPJmLTransform <- function(lpjmlversion = "lpjml5.10.0-m4",
                               climatetype  = "MRI-ESM2-0:ssp245",
                               subtype      = "pnv:soilc",
                               subdata      = NULL,
                               stage        = "smoothed:cut",
                               monthly      = FALSE) {

  ########## CONFIGURE READ START ##########
  cfg        <- toolLPJmLScenario(lpjmlversion = lpjmlversion,
                                  climatetype  = climatetype,
                                  subtype      = subtype)
  readinName <- paste(cfg$version, cfg$climatetype,
                      cfg$subtype, sep = ":")
  readinHist <- paste(cfg$version, sub(":[^:]+$", ":historical", cfg$climatetype),
                      cfg$subtype, sep = ":")

  # read in LPJmL data from selected run (historical or scenario)
  x    <- readSource("LPJmL", subtype = readinName, convert = "onlycorrect")
  unit <- madrat::getFromComment(x, "unit")

  if (!is.null(getItems(x, dim = 2))) {
    if (!grepl("historical", cfg$climatetype)) {
      if (grepl("picontrol", cfg$climatetype)) {
        # read in entire time series of preindustrial control run
        cutYr <- NULL
      } else {
        # cut time series and combine historical with scenario time series
        cutYr <- 1951
        x <- mbind(readSource("LPJmL", subtype = readinHist, convert = "onlycorrect"),
                   x)
      }
    } else {
      # cut time series at given year
      cutYr <- 1930
    }
  }

  # shorten the time series
  if (grepl("cut", stage)) {
    years <- getYears(x, as.integer = TRUE)
    x <- x[, years[years >= cutYr], ]
  }

  if (!is.null(subdata)) {
    if (!all(subdata %in% getNames(x))) {
      stop(paste0("Subdata items '", subdata, "' are not part of selected LPJmL subtype!"))
    }
    x <- x[, , subdata]
  }
  ########## CONFIGURE READ END ##########

  ########## DATA TRANSFORMATION START ###############
  # The standard unit conversion (e.g., from gC/m^2 to tC/ha, from mm to m^3/ha)
  # takes place in correctLPJmL
  # The following transformations are for selected inputs (e.g., those that
  # need to be transformed to tDM)

  if (grepl("gpp|npp|harvestc", subtype)) {
    # Transformation from carbon to dry matter content (from tC/ha to tDM/ha)
    x    <- x / 0.45
    unit <- "tDM/ha"
  }
  ########## DATA TRANSFORMATION STOP ###############

  ######### (optional) monthly handling / aggregation ########
  hasMonth <- any(names(dimnames(x)) == "month")

  # If monthly = TRUE but no month dimension
  if (isTRUE(monthly) && !hasMonth) {
    stop("monthly = TRUE requested, but input data do not contain a 'month' dimension.")
  }

  if (hasMonth) {

    # If monthly values exist and user requested monthly object
    if (isTRUE(monthly)) {
      # rename month labels
      mi <- suppressWarnings(as.integer(getItems(x, dim = "month")))
      if (anyNA(mi)) {
        stop("Month labels are not numeric (1-12); cannot rename automatically.")
      }
      getItems(x, dim = "month") <- tolower(month.abb)[mi]

    } else {
      # If aggregation requested, method must be chosen by user
      parts  <- strsplit(as.character(monthly), ":", fixed = TRUE)[[1]]
      method <- if (length(parts) >= 2) tolower(parts[2]) else NA_character_

      if (is.na(method)) {
        # default: do nothing (useful if data are already yearly)
      } else if (grepl("sum", method)) {
        # fluxes should be summed
        x <- dimSums(x, dim = "month")
      } else if (grepl("mean", method)) {
        # stocks should be averaged (unweighted mean over available months)
        nmonths <- length(getItems(x, dim = "month"))
        x <- dimSums(x, dim = "month") / nmonths
      } else {
        stop("Please specify how monthly values should be aggregated to yearly values ('sum' or 'mean').")
      }
    }
  }

  ########## STAGE HANDELING START  ###############
  if (grepl("smoothed", stage)) {
    # betr has an eight-year harvest rotation. Smoothing these harvest pules
    # directly leads to distorted values, in particular at the beginnging and
    # end. Thus, we first annualise these eight-year values before smoothing.
    if ("biomass tree" %in% unlist(getItems(x, 3, split = TRUE))) {
      x[, , "biomass tree"] <- toolAnnualiseRotation(x[, , "biomass tree"], rotation = 8)
    }
    x <- toolSmooth(x)
  } else if (grepl("raw", stage)) {
    # nothing needs to be done
  } else {
    stop("Stage argument not supported!")
  }
  ########## STAGE HANDELING STOP  ###############

  # Define minimum threshold for returned data
  if (grepl("npp", subtype)) {
    # For NPP negative values are plausible, so no minimum value is set
    minThreshold <- NULL
  } else {
    # For all other data, the minimum threshold is set to 0 to
    # detect potential data inconsistencies
    minThreshold <- 0
  }

  return(list(x            = x,
              weight       = NULL,
              unit         = unit,
              min          = minThreshold,
              description  = paste0("Output from LPJmL (", subtype, ") for ",
                                    lpjmlversion, " and ", climatetype,
                                    " at stage: ", stage, "."),
              isocountries = FALSE))
}
