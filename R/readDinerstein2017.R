#' @title readDinerstein2017
#' @description Reads the RESOLVE Terrestrial Ecoregions 2017 (Dinerstein et al. 2017)
#' and returns the share of a biome group in the land area (area of all ecoregions) of each
#' 0.5 degree grid cell.
#'
#' @param subtype Biome group whose land area share is returned. Currently only
#' \code{"grassland"} = RESOLVE biomes 7 (tropical and subtropical grasslands, savannas and
#' shrublands), 8 (temperate grasslands, savannas and shrublands), 9 (flooded grasslands and
#' savannas) and 10 (montane grasslands and shrublands).
#'
#' @return magpie object (67420 cells) with the land area share of the selected biome group.
#' Cells outside any ecoregion of that group (NA, or NaN without any ecoregion) are set to zero in
#' \code{\link{correctDinerstein2017}}.
#' @author Florian Humpenoeder
#' @seealso \code{\link{correctDinerstein2017}}
#' @examples
#' \dontrun{
#' readSource("Dinerstein2017", subtype = "grassland", convert = "onlycorrect")
#' }
#'
#' @importFrom mstools toolGetMappingCoord2Country
#' @importFrom withr local_tempdir defer

readDinerstein2017 <- function(subtype = "grassland") {

  biomeGroups <- list(grassland = c(7, 8, 9, 10))
  if (!subtype %in% names(biomeGroups)) {
    stop("Unknown subtype. Available: ", paste(names(biomeGroups), collapse = ", "))
  }

  # large vector source: keep terra scratch files on disk, not in memory
  terra::terraOptions(tempdir = local_tempdir(tmpdir = getConfig("tmpfolder")), todisk = TRUE, memfrac = 0.5)
  defer(terra::terraOptions(tempdir = tempdir()))

  ecoregions <- terra::vect("Ecoregions2017.shp")
  grid <- terra::rast(resolution = 0.5, xmin = -180, xmax = 180, ymin = -90, ymax = 90, crs = "EPSG:4326")

  # cover = TRUE gives the fraction of the whole grid cell (including water) covered by the polygons
  # (ecoregions do not overlap); dividing by the cover of all ecoregions gives the share of the land area
  biomes <- ecoregions[ecoregions$BIOME_NUM %in% biomeGroups[[subtype]], ]
  biomeCover <- terra::rasterize(biomes, grid, cover = TRUE)
  landCover <- terra::rasterize(ecoregions, grid, cover = TRUE)
  coverFrac <- terra::clamp(biomeCover / landCover, upper = 1)

  map <- toolGetMappingCoord2Country(pretty = TRUE)
  out <- as.magpie(terra::extract(coverFrac, map[c("lon", "lat")], ID = FALSE)[, 1], spatial = 1)
  dimnames(out) <- list("x.y.iso" = paste(map$coords, map$iso, sep = "."), "t" = NULL, "data" = subtype)

  return(out)
}
