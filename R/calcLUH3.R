#' @title calcLUH3
#' @description Prepares the LUH3 historic landuse-dataset for usage in MAgPIE.
#'
#' @param landuseTypes magpie: magpie landuse classes,
#'                     LUH3: original landuse classes
#' @param irrigation    if true: areas are returned separated by irrigated and rainfed,
#'                      if false: total areas (irrigated + rainfed)
#' @param cellular      if true: dataset is returned on 0.5 degree resolution,
#'                      if false: return country-level data
#' @param yrs           years to be returned
#'
#' @return magpie object with land data in Mha
#'
#' @author Wanderson Costa, Pascal Sauer, Miodrag Stevanovic, Alexandre Koberle
#' @seealso
#' [calcLanduseInitialisation()]
#' @examples
#' \dontrun{
#' calcOutput("LUH3")
#' }
calcLUH3 <- function(landuseTypes = "magpie", irrigation = FALSE,
                     cellular = FALSE, yrs = seq(1965, 2020, 5)) {

  .aggregateWithMapping <- function(x) {
    mapping <- calcOutput("ResolutionMapping", input = "magpie", target = "luh3", aggregate = FALSE)
    mapping$x.y.iso <- paste0(mapping$cellOriginal, ".", mapping$country)
    mapping <- mapping[, c("cell", "x.y.iso")]

    x <- toolAggregate(x, mapping)
    names(dimnames(x))[1] <- "x.y.iso"

    return(x)
  }

  .ensureAllCells <- function(x, clustermap) {
    missingCells <- setdiff(clustermap$cell, getItems(x, 1))
    x <- magclass::add_columns(x, missingCells, dim = 1, fill = 0)
    stopifnot(setequal(clustermap$cell, getItems(x, 1)))
    x <- x[clustermap$cell, , ]
    return(x)
  }

  clustermap <- readSource("MagpieFulldataGdx", subtype = "clustermap")

  if (!landuseTypes %in% c("magpie", "LUH3")) {
    stop("Unknown landuseTypes = \"", landuseTypes, "\", allowed are: magpie, LUH3")
  }

  states <- readSource("LUH3", "states", yrs)
  stopifnot(terra::units(states) == "Mha")
  # skip plantations (forestry) as it's all zeros in LUH3 at the moment
  states <- states[[grep("pltns", names(states), invert = TRUE)]]
  states <- as.magpie(states)

  x <- .aggregateWithMapping(states)
  getItems(x, 2) <- sub("-.+", "", getItems(x, 2))
  x <- .ensureAllCells(x, clustermap)

  names(dimnames(x)) <- c("x.y.iso", "t", "landuse")
  getSets(x, fulldim = FALSE)[3] <- "landuse"

  if (isTRUE(irrigation)) {
    crops <- c("c3ann", "c3per", "c4ann", "c4per", "c3nfx")
    irrigLUH <- readSource("LUH3", "management", yrs, convert = FALSE)
    irrigLUH <- as.magpie(irrigLUH)
    irrigLUH <- irrigLUH[, , paste0("irrig_", crops)]
    stopifnot(0 <= irrigLUH, irrigLUH <= 1)

    getNames(irrigLUH) <- crops
    # convert to Mha by multiplying with cropland in Mha
    irrigLUH <- irrigLUH * states[, , crops]

    irrigLUH <- .aggregateWithMapping(irrigLUH)
    irrigLUH <- .ensureAllCells(irrigLUH, clustermap)

    names(dimnames(irrigLUH)) <- c("x.y.iso", "t", "data")

    x <- add_dimension(x, dim = 3.2, add = "irrigation", nm = "total")
    getItems(x, 3.2, full = TRUE)[getItems(x, 3.1, full = TRUE) %in% crops] <- "rainfed"
    x <- add_columns(x, dim = 3, addnm = paste0(crops, ".irrigated"))

    irrigLUH <- add_dimension(irrigLUH, dim = 3.2, add = "irrigation", nm = "irrigated")
    x[, , getItems(irrigLUH, 3)] <- irrigLUH

    x[, , "rainfed"] <- x[, , "rainfed"] - collapseNames(x[, , "irrigated"])
    stopifnot(min(x[, , "rainfed"]) >= 0)
  }

  if (landuseTypes == "magpie") {
    mapping <- toolGetMapping("LUH3.csv", where = "mrlandcore")
    if (isTRUE(irrigation)) {
      mapping <- data.frame(luh3 = getItems(x, 3),
                            land = paste0(mapping[match(getItems(x, 3.1, full = TRUE), mapping$luh3), ]$land,
                                          ".", getItems(x, 3.2, full = TRUE)))
    }
    stopifnot(setequal(getItems(x, 3), mapping$luh3))
    x <- toolAggregate(x, mapping, dim = 3, from = "luh3", to = "land")
  }

  if (!cellular) {
    x <- mstools::toolConv2CountryByCelltype(x, cells = "lpjcell")
  }

  return(list(x            = x,
              weight       = NULL,
              unit         = "Mha",
              description  = "land area for different land use types",
              isocountries = !cellular))
}
