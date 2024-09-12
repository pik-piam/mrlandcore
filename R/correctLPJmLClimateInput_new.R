#' @title correctLPJmLClimateInput_new
#' @description Correct LPJmL climate input variables
#'
#' @param x magpie object provided by the read function
#'
#' @return Magpie objects with results on cellular level, weight, unit and description.
#' @author Marcos Alves, Felicitas Beier
#'
#' @seealso
#' \code{\link{readLPJmLClimateInput_new}}
#' @examples
#'
#' \dontrun{
#' readSource("LPJmLClimateInput_new", subtype, convert="onlycorrect")
#' }
#'
#' @import magclass
#' @importFrom madrat toolConditionalReplace

correctLPJmLClimateInput_new <- function(x) { # nolint

  x <- toolConditionalReplace(x,
                              conditions = c("is.na()"),
                              replaceby = 0)

  return(x)
}
