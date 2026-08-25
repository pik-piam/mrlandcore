#' @title calcFallow
#' @description Returns fallow land calculated based on physical cropland extent
#'              and harvested area output of default croparea datasource.
#'              The resultant areas are scaled to match the land croparea land
#'              class of calcLanduseInitialisation.
#' @param cellular TRUE for cellular outputs
#' @return MAgPIE object containing fallow land in Mha
#' @author David Hoetten, Felicitas Beier, Benjamin Bodirsky
#' @seealso
#' \code{\link{readLandInG}}
#' @examples
#' \dontrun{
#' calcOutput("Fallow")
#' }
#' @importFrom magclass dimSums mbind
#' @importFrom madrat toolConditionalReplace
#'
calcFallow <- function(cellular = FALSE) {

  fallow <- calcOutput("Croparea", cellular = cellular,
                       physical = TRUE, fallow = TRUE,
                       irrigation = FALSE, aggregate = FALSE)
  fallow <- fallow[, , "fallow"]

  return(list(x = fallow,
              weight = NULL,
              description = "Fallow land",
              unit = "Mha",
              isocountries = !cellular))

}
