#' @title toolCellArea
#' @description Calculates cell area (in Mha) for a given object
#'
#' @param x magpie object with coordinate data
#' @return mag object
#' @author Kristine Karstens, Felicitas Beier
#' @export

toolCellArea <- function(x) {

  if (!magclass::hasCoords(x)) {
    stop("Coordinate data is expected (called `x` and `y`) by toolCellArea")
  }

  if (length(getItems(x, dim = 1)) != 67420) {
    stop("Half-degree data with 67420 cells is expected by toolCellArea")
  }

  coords    <- getItems(x, dim = c("x", "y"), full = TRUE)
  coords    <- lapply(coords, function(z) as.numeric(gsub("p", ".", z)))
  # Transform: square meter -> Mha (1ha = 10000m^2)
  cellArea  <- (111e3 * 0.5) * (111e3 * 0.5) * cos(coords$y / 180 * pi) / 1e+10

  cellArea  <- magclass::clean_magpie(magclass::as.magpie(cellArea, spatial = 1))
  magclass::getSets(cellArea, fulldim = FALSE)[1:2] <- magclass::getSets(x, fulldim = FALSE)[1:2]
  magclass::getItems(cellArea, dim = 1, raw = TRUE) <- magclass::getItems(x, dim = 1)

  return(cellArea)
}
