#' @title calcLUH2MAgPIE
#' @description Calculates the real aggregation of LUH croptypes to MAgPIE croptypes
#'              out of LUH2FAO and FAO2MAgPIE mappings
#'
#' @param share       total (for total numbers), LUHofMAG (for share of LUH within kcr types),
#'                    MAGofLUH (for share of kcr within LUH types)
#' @param bioenergy   "ignore": 0 for share and totals,
#'                    "fix": fixes betr and begr shares in LUHofMAG to 1 for c3per and c4per
#' @param rice        rice category: "non_flooded" or "total"
#' @param missing     "ignore" will leave data as is,
#'                    "fill" will add proxy values for data gaps of FAO
#' @return List of magpie objects with results on country level, weight on country level, unit and description
#' @author Kristine Karstens, Felicitas Beier
#' @examples
#' \dontrun{
#' calcOutput("LUH2MAgPIE")
#' }
#'
#' @importFrom magpiesets findset

calcLUH2MAgPIE <- function(share = "total", bioenergy = "ignore",
                           rice = "non_flooded", missing = "ignore") {

  # proxy filling map in the case of `missing = "fill"`
  proxyMapping <- c(
    # Polar or Sub-polar with Iceland/Norway
    ATF = "ISL", FLK = "ISL", GRL = "ISL", SGS = "ISL", HMD = "ISL", SJM = "NOR",
    # Nordic or Baltic
    ALA = "FIN",
    # North Atlantic with France or UK or Canada
    GGY = "FRA", JEY = "FRA", IMN = "GBR", SPM = "CAN",
    # Middle East
    PSE = "ISR",
    # Caribbean with Dominican Republic or Cuba or Venezuela or USA
    AIA = "DOM", MSR = "DOM", GLP = "DOM", MTQ = "DOM", VIR = "DOM",
    CYM = "CUB", TCA = "CUB", CUW = "VEN", BMU = "USA",
    # Indian Ocean with Sri Lanka or Indonesia
    IOT = "LKA", CCK = "IDN", CXR = "IDN",
    # South America/Africa
    GUF = "SUR", ESH = "MAR", MYT = "MDG", REU = "MDG", SHN = "ZAF",
    # Pacific with Philippines or PNG or Australia or New Zealand
    ASM = "PHL", GUM = "PHL", MHL = "PHL", MNP = "PHL", PLW = "PHL", UMI = "PHL", WLF = "PHL",
    FSM = "PNG", NFK = "AUS", PCN = "NZL"
  )

  if (share == "total") {

    if (missing == "fill") {
      warning("No missing data for total numbers assumed.")
    }

    FAOdata     <- calcOutput("Croparea", sectoral = "ProductionItem", # nolint : object_name_linter.
                              physical = FALSE, aggregate = FALSE)

    if (rice == "non_flooded") {
      # Rice areas are pre-determined by areas reported as flooded in LUH.
      # All additional rice areas (according to FAO) are allocated using FAO data
      nonfloodedShr  <- calcOutput("Ricearea", cellular = FALSE, share = TRUE, aggregate = FALSE)
      commonYears    <- intersect(getYears(nonfloodedShr), getYears(FAOdata))
      nonfloodedShr  <- nonfloodedShr[, commonYears, ]
      FAOdata        <- FAOdata[, commonYears, ]                       # nolint : object_name_linter
      FAOdata[, , "27|Rice"] <- FAOdata[, , "27|Rice"] * nonfloodedShr # nolint : object_name_linter.

    }

    kcr         <- findset("kcr")
    mapping     <- toolGetMapping("FAO2LUH2MAG_croptypes.csv", type = "sectoral", where = "mrlandcore")

    aggregation <- toolAggregate(FAOdata, rel = mapping, from = "ProductionItem",
                                 to = "LUH2kcr", dim = 3.1, partrel = TRUE)
    aggregation <- add_columns(aggregation, addnm = c("betr", "begr"), dim = 3.2)
    aggregation <- aggregation[, , kcr]
    aggregation <- complete_magpie(collapseNames(aggregation), fill = 0)
    aggregation[which(is.na(aggregation))] <- 0
    getSets(aggregation, fulldim = FALSE)  <- c("ISO", "Year", "LUH.MAG")

    x    <- aggregation
    unit <- "million ha"

  } else if (share == "LUHofMAG") {

    aggregation <- calcOutput("LUH2MAgPIE", aggregate = FALSE, rice = rice)

    MAG  <- dimSums(aggregation, dim = "LUH") # nolint : object_name_linter.
    x    <- aggregation / MAG
    x[which(is.na(x))] <- 0
    unit <- "share of area"

    if (bioenergy == "fix") {

      x[, , "c3per.betr"] <- 1
      x[, , "c4per.begr"] <- 1

    } else if (bioenergy != "ignore") {
      stop("2nd generation bioenergy setting not supported")
    }

    if (missing == "fill") {
      # check for countries/years where no data is reported from FAO and fill with proxy of similar country
      noData       <- where(dimSums(toolIso2CellCountries(x), dim = 3) == 0)$true$individual
      for (i in row(noData)[, 1]) {
        x[noData[i, "ISO"], noData[i, "Year"], ]  <- x[proxyMapping[noData[i, "ISO"]], noData[i, "Year"], ]
      }

      # check for countries/years/croptypes where no data is reported from FAO and fill with default values
      noData     <- where(dimSums(x, dim = 3.1) == 0)$true$individual
      meanValues <- dimSums(x * dimSums(aggregation, dim = "LUH"), dim = "ISO") /
        dimSums(aggregation, dim = c("ISO", "LUH"))
      meanValues[is.nan(meanValues)] <- 0
      for (i in row(noData)[, 1]) {
        x[noData[i, "ISO"], noData[i, "Year"], noData[i, "MAG"]]  <- meanValues[, noData[i, "Year"], noData[i, "MAG"]]
      }

      # consistency check
      if (any(round(dimSums(x, dim = 3.1), 4) != 1)) {
        warning("Not all factors could been filled, even though 'missing' was set to 'fill'.")
      }
    }

  } else if (share == "MAGofLUH") {

    aggregation <- calcOutput("LUH2MAgPIE", aggregate = FALSE, rice = rice)

    LUH  <- dimSums(aggregation, dim = "MAG") # nolint : object_name_linter.
    x    <- aggregation / LUH
    x[which(is.na(x))] <- 0
    unit <- "share of area"

    if (bioenergy != "ignore") {
      stop("2nd generation bioenergy setting not supported")
    }

    if (missing == "fill") {
      # check for countries/years where no data is reported from FAO and fill with proxy
      noData       <- where(dimSums(toolIso2CellCountries(x), dim = 3) == 0)$true$individual
      for (i in row(noData)[, 1]) {
        x[noData[i, "ISO"], noData[i, "Year"], ]  <- x[proxyMapping[noData[i, "ISO"]], noData[i, "Year"], ]
      }

      # check for countries/years/croptypes where no data is reported from FAO and fill with default values
      noData       <- where(dimSums(x, dim = 3.2) == 0)$true$individual
      meanValues   <- dimSums(x * dimSums(aggregation, dim = "MAG"), dim = "ISO") /
        dimSums(aggregation, dim = c("ISO", "MAG"))
      for (i in row(noData)[, 1]) {
        x[noData[i, "ISO"], noData[i, "Year"], noData[i, "LUH"]]  <- meanValues[, noData[i, "Year"], noData[i, "LUH"]]
      }

      # consistency check
      if (any(round(dimSums(x, dim = 3.2), 4) != 1)) {
        warning("Not all factors could been filled, even though 'missing' was set to 'fill'.")
      }
    }

  } else {
    stop("Share type not supported")
  }

  return(list(x           = x,
              weight      = NULL,
              unit        = unit,
              description = "Relation matrix for LUH croptype and MAgPIE croptype areas"))
}
