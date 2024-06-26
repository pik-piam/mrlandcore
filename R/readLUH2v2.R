#' @title readLUH2v2
#' @description read LUH inputs
#'
#' @param subtype switch between different inputs
#'
#' @return List of magpie objects with results on cellular level, weight, unit and description
#' @author Florian Humpenoeder, Stephen Wirth, Kristine Karstens, Felicitas Beier,
#' Jan Philipp Dietrich, Patrick v. Jeetze
#'
#' @importFrom ncdf4 nc_open
#' @importFrom terra rast ext subset aggregate project ext<- terraOptions
#' @importFrom magclass as.magpie mbind
#' @importFrom withr local_tempdir defer
#' @importFrom stringr str_match str_count str_subset

readLUH2v2 <- function(subtype) {
  # set terra options and temporary directory
  terraOptions(tempdir = local_tempdir(tmpdir = getConfig("tmpfolder")), todisk = TRUE, memfrac = 0.25)
  defer(terraOptions(tempdir = tempdir()))

  # basic settings
  timeSel <- seq(1901, 2015, by = 1)
  offset  <- 849  # year 850=1, year 1900=1051, year 2015=1166
  # grep years to set other than default years, if subtypes ends with '_850to1901' like time span expression
  timeSpan <- str_match(subtype, "_(\\d+)to(\\d+)")[2:3]
  if (all(!is.na(timeSpan))) {
    timeSel <- seq(timeSpan[1], timeSpan[2], by = 1)
    subtype <- gsub("_(\\d+)to(\\d+)", "", subtype)
  }

  # File to process
  fStates <- "states.nc"
  fMan    <- "management.nc"
  fTrans  <- "transitions.nc"

  ### Define dimensions
  map <- toolGetMappingCoord2Country(pretty = TRUE)

  if (grepl("states", subtype)) {
    # Open file and process information
    ncFile <- nc_open(fStates)
    data   <- setdiff(names(ncFile$var), c("secma", "secmb", "lat_bounds", "lon_bounds"))
    # Land area
    carea      <-  suppressWarnings(rast("staticData_quarterdeg.nc", subds = "carea"))
    ext(carea) <- c(-180, 180, -90, 90)

    x  <- NULL
    for (item in data) {
      # read in share of land type
      shr <- suppressWarnings(subset(rast(fStates, subds = item), timeSel - offset))
      checkSum <- terra::global(shr * carea, sum, na.rm = TRUE)
      # aggregate from 0.25 degree to 0.5 degree
      mag <- terra::aggregate(shr * carea, fact = 2, fun = sum, na.rm = TRUE)
      # Check whether sum before and after aggregation is the same.
      # Note: unit is km^2, so only rounded to first digit
      if (any(round(checkSum - terra::global(mag, sum, na.rm = TRUE), digits = 1) != 0)) {
        stop("There is an issue with the aggregation. Please check mrcommons::readLUH2v2")
      }
      # transform to MAgPIE object and clean up
      mag <- as.magpie(terra::extract(mag, map[c("lon", "lat")])[, -1], spatial = 1, temporal = 2)
      getNames(mag) <- item
      getCells(mag) <- paste(map$coords, map$iso, sep = ".")
      getYears(mag) <- timeSel
      getSets(mag)  <- c("x.y.iso", "t", "data")
      x <- mbind(x, mag)
    }

    # Convert from km^2 to Mha
    x <- x / 10000

  } else if (grepl("transition", subtype)) {
    # Open file and process information
    ncFile  <- nc_open(fTrans)
    luTrans <- setdiff(names(ncFile$var), c("secma", "secmb", "lat_bounds", "lon_bounds"))
    luTrans <- grep("to", luTrans, value = TRUE)

    lu <- list(crop  = c("c3ann", "c3per", "c4ann", "c4per", "c3nfx"),
               past  = c("pastr", "range"),
               nat   = c("primf", "primn", "secdf", "secdn"),
               urban = c("urban"))

    luTransReduced <- luTrans
    for (i in seq_along(lu)) {
      luTransReduced <- gsub(paste(lu[[i]], collapse = "|"), names(lu[i]), luTransReduced)
    }

    zeroTrans <- grepl(paste(paste(names(lu), names(lu), sep = "_to_"),
                             collapse = "|"), luTransReduced)
    # Land area
    carea      <- suppressWarnings(rast("staticData_quarterdeg.nc", subds = "carea"))
    ext(carea) <- c(-180, 180, -90, 90)

    x <- new.magpie(map$coords, timeSel, unique(luTransReduced[!zeroTrans]), fill = 0)

    for (item in seq_along(luTrans)) {
      # This attributes LUC to the year resulting from it
      print(luTrans[item])
      if (!zeroTrans[item]) {
        shr <- suppressWarnings(subset(rast(fTrans, subds = luTrans[item]), timeSel - offset - 1))
        checkSum <- terra::global(shr * carea, sum, na.rm = TRUE)
        # aggregate from 0.25 degree to 0.5 degree
        mag <- terra::aggregate(shr * carea, fact = 2, fun = sum, na.rm = TRUE)
        # Check whether sum before and after aggregation is the same.
        # Note: unit is km^2, so only rounded to first digit
        if (any(round(checkSum - terra::global(mag, sum, na.rm = TRUE), digits = 1) != 0)) {
          stop("There is an issue with the aggregation. Please check mrcommons::readLUH2v2")
        }
        # Transform to MAgPIE object
        mag <- as.magpie(terra::extract(mag, map[c("lon", "lat")])[, -1], spatial = 1, temporal = 2)
        getNames(mag) <- luTransReduced[item]
        getCells(mag) <- paste(map$coords, map$iso, sep = ".")
        getYears(mag) <- timeSel
        getSets(mag)  <- c("x.y.iso", "t", "data")
        x[, , luTransReduced[item]] <- collapseNames(x[, , luTransReduced[item]] + mag)
      }
    }

    getCells(x)  <- getCells(mag)
    getSets(x)   <- getSets(mag)

    # Convert from km^2 to Mha
    x <- x / 10000

  } else if (grepl("irrigation", subtype)) {
    # Mapping between states and management_irrigation
    dataMan    <- c("irrig_c3ann", "irrig_c3per", "irrig_c4ann", "irrig_c4per", "irrig_c3nfx", "flood")
    dataStates <- c("c3ann",       "c3per",       "c4ann",       "c4per",       "c3nfx",       "c3ann")
    data       <- matrix(data = c(dataMan, dataStates), ncol = 2)

    # Land area
    carea         <- suppressWarnings(rast("staticData_quarterdeg.nc", subds = "carea"))
    ext(carea) <- c(-180, 180, -90, 90)

    x  <- NULL
    for (item in dataMan) {
      shr   <- suppressWarnings(subset(rast(fStates, subds = data[data[, 1] == item, 2]), timeSel - offset))
      irShr <- suppressWarnings(subset(rast(fMan,    subds = item), timeSel - offset))
      # grid cell fraction of crop area x grid cell area x irrigated fraction of crop area
      tmp <- shr
      for (i in seq_len(dim(tmp)[3])) {
        tmp[[i]] <- shr[[i]] * carea * irShr[[i]]
      }
      checkSum <- terra::global(tmp, sum, na.rm = TRUE)
      # aggregate from 0.25 degree to 0.5 degree
      mag <- terra::aggregate(tmp, fact = 2, fun = sum, na.rm = TRUE)
      # Check whether sum before and after aggregation is the same.
      # Note: unit is km^2, so only rounded to first digit
      if (any(round(checkSum - terra::global(mag, sum, na.rm = TRUE), digits = 1) != 0)) {
        stop("There is an issue with the aggregation. Please check mrcommons::readLUH2v2")
      }
      # Transform to MAgPIE object
      mag <- as.magpie(terra::extract(mag, map[c("lon", "lat")])[, -1], spatial = 1, temporal = 2)
      getNames(mag) <- item
      getYears(mag) <- timeSel
      getCells(mag) <- paste(map$coords, map$iso, sep = ".")
      getSets(mag)  <- c("x.y.iso", "t", "data")
      x <- mbind(x, mag)
    }

    # Convert from km^2 to Mha
    x <- x / 10000

  } else if (grepl("ccode", subtype)) {
    # Load raster data on 0.25° and extend to full grid
    ccode25      <- suppressWarnings(rast("staticData_quarterdeg.nc", subds = "ccode"))
    ext(ccode25) <- c(-180, 180, -90, 90)

    # Create new raster object on 0.5° and re-project 0.25°-raster on 0.5°-raster
    r50     <- rast(res = 0.5)
    ccode50 <- project(ccode25, r50, method = "near") # re-project to regular grid

    x <- as.magpie(terra::extract(ccode50, map[c("lon", "lat")])[, -1], spatial = 1)
    getYears(x) <- 2000
    getNames(x) <- "ccode"
    getCells(x) <- paste(map$coords, map$iso, sep = ".")
    getSets(x)  <- c("x.y.iso", "t", "ccode")
  }

  return(clean_magpie(x))
}
