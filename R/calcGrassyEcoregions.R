#' @title calcGrassyEcoregions
#' @description Grid cell areal cover fraction of the RESOLVE Ecoregions 2017 grassy biomes
#' (Dinerstein et al. 2017): tropical and subtropical, temperate, flooded, and montane
#' grasslands, savannas and shrublands (biomes 7-10). Used to correct the LPJmL-derived
#' potential forest area, which overestimates forest cover in these open ecosystems,
#' where tree planting and forest expansion are ecologically inappropriate (Veldman et al. 2015).
#'
#' @return magpie object in cellular resolution (67420 lpjcells)
#' @author Florian Humpenoeder
#' @examples
#' \dontrun{
#' calcOutput("GrassyEcoregions", aggregate = FALSE)
#' }
#'
#' @importFrom madrat readSource calcOutput
#' @importFrom magclass dimSums setYears getNames<-

calcGrassyEcoregions <- function() {

  grassy <- readSource("Dinerstein2017", subtype = "grassland", convert = "onlycorrect")
  getNames(grassy) <- NULL

  # land area weights the cluster/region aggregation of the cover fraction
  # (LUH3 land area, not calcLandArea: that one derives from calcLanduseInitialisation,
  # which uses this function via toolForestRelocate)
  landArea <- calcOutput("LUH3", landuseTypes = "LUH3", irrigation = FALSE, cellular = TRUE,
                         yrs = 1995, aggregate = FALSE)
  landArea <- setYears(dimSums(landArea, dim = 3), NULL)

  return(list(
    x           = grassy,
    weight      = landArea,
    unit        = "share",
    description = "Grassland ecoregions cover fraction (RESOLVE 2017 biomes 7-10)",
    isocountries = FALSE
  ))
}
