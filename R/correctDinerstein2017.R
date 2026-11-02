#' @title correctDinerstein2017
#' @description Sets missing values (cells outside any ecoregion of the selected biome
#' group) to zero.
#' @param x magpie object provided by \code{\link{readDinerstein2017}}
#' @return magpie object on cellular level
#' @author Florian Humpenoeder
#' @seealso \code{\link{readDinerstein2017}}
#' @examples
#' \dontrun{
#' readSource("Dinerstein2017", subtype = "grassland", convert = "onlycorrect")
#' }
#'
#' @importFrom madrat toolConditionalReplace

correctDinerstein2017 <- function(x) {
  x <- toolConditionalReplace(x, conditions = "is.na()", replaceby = 0)

  return(x)
}
