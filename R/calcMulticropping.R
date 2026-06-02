#' @title calcMulticropping
#' @description Multicropping factor, telling how many harvests are produced
#' on one unit of physical area. Fallow lands are not counted in, so the factor
#' is always larger than 1.
#'
#' @param irrigation report irrigated and non-irrigated areas separately
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky, Felicitas Beier
#' @seealso
#' [calcCroparea()]
#' @examples
#' \dontrun{
#' calcOutput("Multicropping")
#' }
#'
calcMulticropping <- function(irrigation = TRUE) {

  phys   <- collapseNames(calcOutput("Croparea", physical = TRUE, cellular = TRUE,
                                             aggregate = FALSE, irrigation = irrigation))
  harv   <- collapseNames(calcOutput("Croparea", physical = FALSE, cellular = TRUE,
                                             aggregate = FALSE, irrigation = irrigation))
  out <- ifelse(phys > 0, harv / phys, NA)

  out[is.na(out)] <- 1

  out  <- toolHoldConstantBeyondEnd(out)
  phys <- toolHoldConstantBeyondEnd(phys)

  return(list(x           = out,
              weight      = phys,
              unit        = "ratio",
              description = "Ratio of area harvested to phyiscal area, excluding fallow land"))
}
