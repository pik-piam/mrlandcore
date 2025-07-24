#' @title calcLUH3
#' @description Integrates the LUH3 landuse-dataset
#'
#' @param landuse_types magpie: magpie landuse classes,
#'                      LUH3: original landuse classes
#'                      flooded: flooded areas as reported by LUH
#' @param irrigation    if true: areas are returned separated by irrigated and rainfed,
#'                      if false: total areas
#' @param cellular      if true: dataset is returned on 0.5 degree resolution
#' @param cells         Switch between "magpiecell" (59199) and "lpjcell" (67420) (default: "lpjcell")
#'                      NOTE: This setting also affects the sums on country level!
#' @param yrs           years to be returned (default: seq(1965, 2020, 5))
#'
#' @return List of magpie objects with results on country level,
#'         weight on country level, unit and description
#'
#' @author Wanderson Costa, Pascal Sauer, Miodrag Stevanovic, Alexandre Koberle
#' @seealso
#' [calcLanduseInitialisation()]
#' @examples
#' \dontrun{
#' calcOutput("LUH3")
#' }
calcLUH3 <- function(landuse_types = "magpie", irrigation = FALSE, # nolint
                     cellular = FALSE, cells = "lpjcell", yrs = seq(1965, 2020, 5)) {

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

  if (!landuse_types %in% c("magpie", "LUH3", "flooded")) {
    stop("Unknown landuse_types = \"", landuse_types, "\", allowed are: magpie, LUH3, flooded")
  }

  if (landuse_types == "flooded") {
    raw <- readSource("LUH3", "management", yrs, convert = FALSE)
    x <- as.magpie(raw[[paste0("y", yrs, "..flood")]])

    x <- .aggregateWithMapping(x)
    x <- .ensureAllCells(x, clustermap)

    names(dimnames(x)) <- c("x.y.iso", "t", "data")
  } else {
    raw <- readSource("LUH3", "states", yrs)
    stopifnot(terra::units(raw) == "Mha")
    # skip plantations (forestry) as it's all zeros in LUH3 at the moment
    raw <- raw[[grep("pltns", names(raw), invert = TRUE)]]
    x <- as.magpie(raw)

    x <- .aggregateWithMapping(x)
    getItems(x, 2) <- sub("-.+", "", getItems(x, 2))
    x <- .ensureAllCells(x, clustermap)

    names(dimnames(x)) <- c("x.y.iso", "t", "landuse")
    getSets(x, fulldim = FALSE)[3] <- "landuse"

    if (isTRUE(irrigation)) {
      rawIrrigLUH <- readSource("LUH3", "management", yrs, convert = FALSE)
      irrigLUH <- as.magpie(rawIrrigLUH)
      variables <- c("irrig_c3ann", "irrig_c3per", "irrig_c4ann",
                     "irrig_c4per", "irrig_c3nfx", "flood")
      irrigLUH <- irrigLUH[, , variables]

      irrigLUH <- .aggregateWithMapping(irrigLUH)
      irrigLUH <- .ensureAllCells(irrigLUH, clustermap)

      names(dimnames(irrigLUH)) <- c("x.y.iso", "t", "data")

      # irrigated areas (excluding flood)
      irrigLUH           <- irrigLUH[, , "flood", invert = TRUE]
      getNames(irrigLUH) <- substring(getNames(irrigLUH), 7)

      x <- add_dimension(x, dim = 3.2, add = "irrigation", nm = "total")
      x <- add_columns(x, dim = 3.2, addnm = c("irrigated", "rainfed"))
      x[, , "irrigated"] <- 0

      irrigLUH <- add_dimension(irrigLUH, dim = 3.2, add = "irrigation", nm = "irrigated")
      x[, , paste(getNames(irrigLUH, dim = 1), "irrigated", sep = ".")] <- irrigLUH

      # rainfed areas
      x[, , "rainfed"] <- collapseNames(x[, , "total"]) - collapseNames(x[, , "irrigated"])

      if (any(x[, , "rainfed"] < 0)) {
        vcat(verbosity = 2, "Flooded/irrigated area larger than rainfed area.
             Irrigation limited to total cropland area.")
        tmp                       <- collapseNames(x[, , "irrigated"])
        tmp[x[, , "rainfed"] < 0] <- collapseNames(x[, , "total"])[x[, , "rainfed"] < 0]
        x[, , "irrigated"] <- tmp
        x[, , "rainfed"]   <- collapseNames(x[, , "total"]) - collapseNames(x[, , "irrigated"])
      }

      if (any(x[, , "rainfed"] < 0)) {
        vcat(verbositiy = 1, "Flooded/irrigated area larger than rainfed area despite fix.")
      }
    }

    if (landuse_types == "magpie") {
      mapping <- toolGetMapping("LUH3.csv", where = "mrlandcore")
      x       <- toolAggregate(x, mapping, dim = 3.1, from = "luh3", to = "land")
    }
  }

  # Return correct cell format for further calculations
  # ATTENTION: depending on the settings this might remove some cells
  #            from the data set!
  if (cellular) {
    if (cells == "magpiecell") {
      x <- toolCoord2Isocell(x, cells = cells)
    }
  } else {
    x <- mstools::toolConv2CountryByCelltype(x, cells = cells)
  }

  return(list(x            = x,
              weight       = NULL,
              unit         = "Mha",
              description  = "land area for different land use types.",
              isocountries = !cellular))
}
