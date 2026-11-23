#' @title correctLPJmL
#' @description corrects data that was read in from LPJmL and converts units
#'              to MAgPIE standard
#' @param x list of magpie object and unit provided by the read function
#' @param subtype Switch between different inputs
#'
#' @author Felicitas Beier, Kristine Karstens
#' @seealso
#' [readLPJmL()]
#' @examples
#' \dontrun{
#' readSource("LPJmL", convert = "onlycorrect")
#' }
#'
correctLPJmL <- function(x, subtype) {

  # Check and replace negative values
  # (except for NPP where negative values are allowed)
  if (!grepl("npp", subtype)) {
    # min(x) >= -1e-10 is equivalent to all(x >= -1e-10) but short-circuits
    # a full elementwise comparison into a single reduction
    noNeg <- min(x) >= -1e-10
    toolExpectTrue(noNeg, "Data provided by LPJmL is not negative",
                   falseStatus = "warn")
    # Correct negative values (set to zero) -- skip the full-object pass
    # entirely when the check above already confirmed nothing to replace
    if (min(x, na.rm = TRUE) < 0) {
      x <- madrat::toolConditionalReplace(x, conditions = "<0", replaceby = 0)
    }
  }

  # Check and replace N/A's (same short-circuit reasoning as above)
  noNA <- !anyNA(x)
  toolExpectTrue(noNA, "Data provided by LPJmL doesn't contain N/A's",
                 falseStatus = "warn")
  if (!isTRUE(noNA)) {
    x <- toolConditionalReplace(x, conditions = c("is.na()"), replaceby = 0)
  }

  # extract unit of data
  unit <- madrat::getFromComment(x, "unit")

  # unit conversion
  if (grepl("gC/m2", unit)) {
    # convert gC/m2 to tC/ha
    # Conversion note:
    # Transformation factor (numerator): 1 g = 1e-6 t
    # Transformation factor (denominator): 1 m^2 = 1e-4 ha
    x <- x * 0.01
    unit <- "tC/ha"
  } else if (grepl("mm", unit)) {
    # convert mm to m^3/ha
    # Conversion note:
    # Def.: mm is equal to liter/m^2
    # Transformation factor (numerator): 1 liter = 1e-3 m^3
    # Transformation factor (denominator): 1 m^2   = 1e-4 ha
    x <- x * 10
    unit <- "m^3/ha"
  } else if (identical(unit, "")) {
    # empty units are replaced with 1 in magclass
    unit <- "1"
  } else if (unit == "m2") {
    # convert m^2 to ha
    # Transformation factor (numerator): 1 m^2 = 1e-4 ha
    x <- x * 1e-4
    unit <- "ha"
  } else if (unit == "hm3/month") {
    # Transformation factor (numerator): 1 cubic hectometer = 1 mio. cubic meter
    unit <- "mio. m^3/month"
  }

  return(list(x = x,
              unit = unit))
}
