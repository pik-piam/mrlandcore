#' @title calcPotentialForestArea
#'
#' @description Calculates the area that can potentially be covered by forests,
#'              based on environmental conditions.
#'
#' @param refData  Determines the reference data that the estimated potential
#'                 forest area is derived from (currently only "lpj")
#' @param cells            magpiecell (59199 cells) or lpjcell (67420 cells)
#' @param countryLevel    Whether output shall be at country level.
#'                        Requires aggregate=FALSE in calcOutput.
#' @param lpjml Defines LPJmL version for crop/grass and natveg specific inputs.
#'              Only relevant, if refData = "lpj".
#' @param climatetype Switch between different GCM climate scenarios.
#'                    Only relevant, if refData = "lpj".
#' @param grassyCorrection If TRUE, the potential forest area is reduced by the grassland-ecoregion
#'                    cover fraction (RESOLVE 2017 biomes 7-10, calcGrassyEcoregions), correcting the
#'                    LPJmL overestimation of forest in open grassy ecosystems.
#' @param share If FALSE (default), the potential forest area (Mha) is returned. It is set to at
#'              least the forest area of the land use initialisation (maximum over all initialisation
#'              years) in each cell, so that it includes all existing forest.
#'              If TRUE, the biophysical potential forest share of the cell land area (0-1) is returned,
#'              without this adjustment and on lpjcell level only. This variant does not depend on
#'              calcLanduseInitialisation and is used there (via \code{\link{toolForestRelocate}}).
#'              Only use it with aggregate = FALSE.
#'
#' @return magpie object on grid cell or country level: potential forest area (Mha) or,
#'         with share = TRUE, potential forest share of the grid cell land area
#' @author Patrick v. Jeetze, Florian Humpenoeder
#'
#' @examples
#' \dontrun{
#' calcOutput("PotentialForestArea", aggregate = FALSE)
#' }
#'
#' @importFrom madrat readSource calcOutput toolCountryFill toolConditionalReplace
#' @importFrom magclass dimSums setYears getNames<-
#' @importFrom mstools toolCoord2Isocell

calcPotentialForestArea <- function(refData = "lpj", countryLevel = FALSE, cells = "lpjcell",
                                    lpjml = c(natveg = "LPJmL4_for_MAgPIE_44ac93de"),
                                    climatetype = "MRI-ESM2-0:ssp370", grassyCorrection = TRUE,
                                    share = FALSE) {
  if (refData != "lpj") stop("refData = \"", refData, "\" is not supported. Use \"lpj\".")

  vegc <- calcOutput("LPJmL_new",
    version = lpjml["natveg"],
    climatetype = climatetype,
    subtype = "vegc", stage = "harmonized2020",
    aggregate = FALSE
  )

  potForest <- toolConditionalReplace(vegc, c("<20", ">=20"), c(0, 1))

  if (grassyCorrection) {
    # reduce the potential forest share by the grassland-ecoregion cover fraction, where LPJmL
    # overestimates forest in open grassy ecosystems
    grassy <- calcOutput("GrassyEcoregions", aggregate = FALSE)
    potForest <- potForest * (1 - setYears(grassy, NULL))
  }
  getNames(potForest) <- NULL

  if (share) {
    if (countryLevel || cells != "lpjcell") stop("share = TRUE is only available for lpjcell level")
    return(list(
      x = potForest,
      weight = NULL,
      unit = "share",
      description = "Potential forest share of cell land area",
      isocountries = FALSE
    ))
  }

  landIni <- calcOutput("LanduseInitialisation",
    aggregate = FALSE, cellular = TRUE, nclasses = "seven",
    input_magpie = TRUE, cells = "lpjcell", round = 6
  )
  landArea <- dimSums(landIni[, 1, ], dim = 3)

  potForestArea <- potForest * setYears(landArea, NULL)

  # the potential forest area includes all forest of the land use initialisation
  forestIni <- dimSums(landIni[, , c("primforest", "secdforest", "forestry")], dim = 3)
  forestMax <- setYears(forestIni[, 1, ], NULL)
  forestMax[] <- apply(as.array(forestIni), c(1, 3), max)
  forestOutPot <- forestMax - potForestArea
  forestOutPot[forestOutPot < 0] <- 0
  potForestArea <- potForestArea + forestOutPot

  if (countryLevel) {
    out <- toolCountryFill(dimSums(potForestArea, dim = c("x", "y")), fill = 0)
  } else {
    if (cells == "magpiecell") {
      out <- toolCoord2Isocell(potForestArea)
    } else if (cells == "lpjcell") {
      out <- potForestArea
    } else {
      stop("Please specify cells argument")
    }
  }

  return(list(
    x = out,
    weight = NULL,
    unit = "Mha",
    description = "Potential forest area",
    isocountries = countryLevel
  ))
}
