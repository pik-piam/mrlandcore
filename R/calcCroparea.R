#' @title calcCroparea
#' @description returns croparea
#'
#' @param sectoral    "kcr" MAgPIE items, and "lpj" LPJmL items
#' @param physical    if TRUE it returns the physical area, with cropareas of multicropping systems being
#'                    scaled to match physical areas, if FALSE the area harvested is returned
#' @param fallow      if TRUE fallow land is returned as element in crop set
#' @param cellular    if TRUE: calculates cellular MAgPIE crop area for all magpie croptypes.
#'                    Crop area from LUH3 crop types (c3ann, c4ann, c3per, c4per, cnfx)
#'                    are mapped to MAgpIE crop types using "FAO2LUH2MAG_croptypes" and doing
#'                    an intermediate step via harvested areas of FAO weight area within a
#'                    specific LUH crop type to divide into MAgPIE crop types.
#' @param irrigation  If true: cellular areas are returned separated
#'                    into irrigated and rainfed (see setup in calcLUH3)
#' @param datasource  Croparea data source (LandInG or FAOLUH).
#'                    Datasource FAOLUH is deprecated and is only kept for
#'                    backwards compatibility.
#'
#' @return areas of individual crops from FAOSTAT and weight
#'
#' @author Felicitas Beier, Benjamin Leon Bodirsky
#'
#' @importFrom magpiesets findset

calcCroparea <- function(sectoral = "kcr", physical = TRUE, fallow = FALSE,
                         cellular = FALSE,
                         irrigation = FALSE, datasource = "LandInG") {

  if (datasource == "LandInG") {
    # description of data to be returned
    description <- paste0(ifelse(physical, "physical ", "harvested "),
                          "croparea from LandInG data set")

    # The following calculations are based on cellular, irrigation and fallow
    # dimensions being included. The selection of what is returned happens
    # further down by a recursive function call
    if (irrigation && cellular && fallow) {
      # read in croparea
      croparea <- calcOutput("CropareaLandInG", sectoral = sectoral, physical = TRUE,
                             cellular = TRUE, irrigation = TRUE,
                             aggregate = FALSE)
      selectyears <- getYears(croparea, as.integer = TRUE)
      # Extend data until 2020
      if (!any(grepl("y2020", getItems(croparea, dim = 2)))) {
        selectyears <- c(selectyears, (tail(selectyears, 1) + 1):2020)
        croparea <- toolHoldConstant(croparea, years = selectyears)
      }

      if (!physical) {
        # calculate crop and irrigation-specific multicropping factor
        cropareaMulti <- calcOutput("CropareaLandInG", sectoral = sectoral, physical = FALSE,
                                    cellular = TRUE, irrigation = TRUE,
                                    aggregate = FALSE)

        if (!any(grepl("y2020", getItems(cropareaMulti, dim = 2)))) {
          cropareaMulti <- toolHoldConstant(cropareaMulti, years = selectyears)
        }

        multiFactor <- cropareaMulti / croparea
        multiFactor <- ifelse(test = is.nan(multiFactor), 1, multiFactor)

        # Cap the multicropping factor at 3 harvests per year. Inconsistencies
        # between the physical and the harvested area data set can produce
        # unrealistically high factors in edge cases, and three harvests per year
        # is the defensible ceiling. Report when this stops being an edge case,
        # i.e. when the physical area affected exceeds 10 Mha globally in a year.
        cappedArea <- dimSums(croparea * (multiFactor > 3), dim = c(1, 3), na.rm = TRUE)
        if (max(cappedArea) > 10) {
          vcat(1, "Multicropping factor exceeded 3 on",
               round(max(cappedArea), digits = 1), "Mha of physical cropland in",
               getItems(cappedArea, dim = 2)[which.max(as.vector(cappedArea))],
               "and was truncated to 3 \n")
        }

        multiFactor <- ifelse(test = multiFactor > 3, 3, multiFactor)
        multiFactor <- add_columns(x = multiFactor, addnm = "fallow",
                                   dim = "crop", fill = 1)

      }

      fallowLand   <- calcOutput("FallowLandInG", cellular = TRUE,
                                 aggregate = FALSE)

      if (!any(grepl("y2020", getItems(fallowLand, dim = 2)))) {
        fallowLand <- toolHoldConstant(fallowLand, years = selectyears)
      }

      ### Integrate fallow into physical land
      # implicit assumption: fallow is the same in physical and harvested area,
      # as multicropping with fallow is not multicropping

      fallowLand <- setNames(fallowLand[, selectyears, ], "fallow")

      totalCroparea <- dimSums(croparea, dim = "crop")
      irrigationShare     <- (totalCroparea[, , c("rainfed", "irrigated")] /
                                dimSums(totalCroparea, dim = "irrigation"))
      irrigationShare <- ifelse(is.nan(irrigationShare), 0, irrigationShare)
      fallowLandIrrigated <- irrigationShare * fallowLand

      cropareaWithFallow <- mbind(croparea, fallowLandIrrigated)

      ### Correction to match LanduseInitialisation cropland area

      # calculate area shares
      cropareaShareGrid <- cropareaWithFallow / dimSums(cropareaWithFallow, dim = c("irrigation", "crop"))
      cropareaShareIso <- dimSums(cropareaWithFallow, dim = c("x", "y")) /
        dimSums(cropareaWithFallow, dim = c("x", "y", "irrigation", "crop"))
      cropareaShareGlo <- dimSums(cropareaWithFallow, dim = 1) /
        dimSums(cropareaWithFallow, dim = c("x", "y", "iso", "irrigation", "crop"))
      cropareaShareIso <- ifelse(is.nan(cropareaShareIso), cropareaShareGlo, cropareaShareIso)
      cropareaShareGrid <- ifelse(is.nan(cropareaShareGrid), cropareaShareIso, cropareaShareGrid)

      # for correction: read LanduseInitialisation cropland area
      landuseIni <- calcOutput(type = "LanduseInitialisation", aggregate = FALSE,
                               cellular = TRUE, selectyears = selectyears)

      landuseIniCrop    <- collapseNames(landuseIni[, , "crop"])

      # rescale
      cropareaCalibrated <- cropareaShareGrid * landuseIniCrop

      ### Check physical cropland matching ###
      # Sanity check (physical croparea + fallow land should match total cropland
      # in LanduseInitialisation after calibration)
      if (any(round(dimSums(cropareaCalibrated, dim = c("crop", "irrigation")) - landuseIniCrop, 6) != 0)) {
        stop("Calibrated physical croparea + fallow land does not match LanduseInitialisation cropland.")
      }

      if (!physical) {
        cropareaCalibrated <- cropareaCalibrated * multiFactor
      }

      croparea    <- cropareaCalibrated

    } else {
      # To speed up, aggregation is done with recursively
      croparea <- calcOutput("Croparea", aggregate = FALSE, sectoral = sectoral,
                             physical = physical, fallow = TRUE,
                             cellular = TRUE, irrigation = TRUE,
                             datasource = "LandInG")
      # Aggregation to iso-level and wrt irrigation
      if (!cellular) {
        # aggregate to countries
        croparea <- dimSums(croparea, dim = c("x", "y"))
        # fill missing countries with 0
        croparea <- toolConditionalReplace(x = toolCountryFill(croparea),
                                           conditions = "is.na()", replaceby = 0)
      }
      if (!irrigation) {
        # aggregate irrigation dimension
        croparea <- dimSums(croparea, dim = "irrigation")
      }
    }

    # return croparea (and optionally fallow land)
    if (!fallow) {
      croparea <- croparea[, , "fallow", invert = TRUE]
      description <- paste0(description, " excluding fallow land.")
    } else {
      description <- paste0(description, " including fallow land.")
    }

  } else if (datasource == "FAOLUH") {

    if (fallow) {
      stop("fallow not implemented for FAOLUH")
    }
    ### For backwards compatibility only ###
    # This chunk can be deleted when croparea update is completed.

    # read in croparea
    croparea <- calcOutput("CropareaFAOLUH", sectoral = sectoral, physical = physical,
                           cellular = cellular, irrigation = irrigation,
                           aggregate = FALSE)

    # description of data to be returned
    description <- paste0(ifelse(physical, "physical ", "harvested "),
                          "croparea from LUH and FAOSTAT")

    # return croparea (and optionally fallow land)
    if (fallow) {
      # LUH3/FAO doesn't provide distinction of fallow land
      fallowLand  <- new.magpie(cells_and_regions = getItems(croparea, dim = 1),
                                years = getItems(croparea, dim = 2),
                                names = "fallow",
                                sets = getSets(croparea)[["d3.1"]],
                                fill = 0)
      croparea    <- mbind(croparea, fallowLand)
      description <- paste0(description, " including fallow land.")
    } else {
      description <- paste0(description, " excluding fallow land.")
    }

  } else {
    stop("Selected data source in calcCroparea unknown.")
  }

  return(list(x            = croparea,
              weight       = NULL,
              unit         = "million ha",
              description  = description,
              isocountries = !cellular))
}
