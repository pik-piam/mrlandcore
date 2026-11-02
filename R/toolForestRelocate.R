#' @title toolForestRelocate
#' @description Reallocates cellular forest information
#'              to better match FAO forest information
#'
#' @param lu        uncorrected landuse initialisation data set (cell level)
#' @param luCountry uncorrected landuse initialisation on country level
#' @param natTarget target natural land allocation on country level
#' @param vegC      vegetation carbon data used as reallocation weight
#' @param potForestShr If TRUE (default), other land converted to forest is taken first from the
#'                  biophysical forest potential headroom of a cell (potential forest area from
#'                  \code{calcPotentialForestArea(share = TRUE)} minus existing forest). Only if this headroom
#'                  is insufficient to match the country target, the remainder is converted outside the
#'                  potential. Existing forest is not affected. If FALSE, the forest potential is ignored.
#' @return List of magpie object with results on cellular level
#'
#' @author Kristine Karstens, Jan Philipp Dietrich, Felicitas Beier, Patrick v. Jeetze
#' @importFrom nleqslv nleqslv
#'
#' @export
toolForestRelocate <- function(lu, luCountry, natTarget, vegC, potForestShr = TRUE) { # nolint

  .arrayReduce <- function(x) {
    # drop dimensions but keep time dimension
    if (dim(x)[3] != 1) stop(".arrayReduce only works with a single data dimension!")
    if (dim(x)[1] == 1) return(array(x, dim = dim(x)[2], dimnames = dimnames(x)[2]))
    return(array(x, dim = dim(x)[1:2], dimnames = dimnames(x)[1:2]))
  }

  forests <- c("primforest", "secdforest", "forestry")
  nature  <- c(forests, "other")

  if (round(sum(lu) - sum(luCountry), 4) != 0) warning("lu and luCountry differ in total land area")
  if (round(sum(lu[, , nature]) - sum(natTarget), 4) != 0) warning("lu and natTarget differ in total land area")

  # store cell area to check later that it remains constant
  luCellArea <- setItems(dimSums(lu[, 1, ], dim = 3), dim = 2, NULL)

  # biophysical potential forest share of the cell area
  potShr <- NULL
  if (isTRUE(potForestShr)) {
    potShr <- calcOutput("PotentialForestArea", share = TRUE, aggregate = FALSE)
    potShr <- toolFillYears(potShr, getYears(natTarget))[getItems(lu, dim = 1), getYears(natTarget), ]
  }

  # reduce, if necessary to FAO
  reduce <- increase <- round(natTarget - luCountry[, , nature], 8)
  reduce[reduce > 0]     <- 0
  increase[increase < 0] <- 0

  # vegetation carbon without time dimension: same values in all target years
  if (is.null(getYears(vegC))) getYears(vegC) <- getYears(natTarget)

  # weight function to determine correct cell weights for area removal
  findWeight <- function(p, cellArea, isoReduction, cellWeight) {
    rowSums(cellArea * (1 - (1 - cellWeight)^p)) + isoReduction + 10^-10
  }

  # area to be taken from the cells of a country (area: cells x years) to achieve the
  # country-level reduction (reduction: <= 0 per year), distributed according to cellWeight
  .removeArea <- function(area, reduction, cellWeight, landType) {
    area <- setNames(area, NULL)
    if (all(reduction == 0)) return(area * 0)
    # check for one cell countries
    if (dim(area)[1] == 1) {
      # trivial case of one cell countries
      return(-as.magpie(reduction))
    }
    if (all(dimSums(area != 0, dim = 1) == 1)) {
      # trivial case in which in each year exactly one cell contains land in the category to be reduced
      return(setNames(-1 * (area != 0) * as.magpie(reduction), NULL))
    }

    removed <- area * 0
    # check for edge case in which all land of that category must be removed and treat it separately
    fullRemoval <- (round(dimSums(area, dim = 1) + as.magpie(reduction), 3) == 0)
    if (any(fullRemoval)) {
      removed[, fullRemoval, ] <- area[, fullRemoval, ]
      reduction[fullRemoval] <- 0
    }

    active <- (reduction != 0)
    if (any(active)) {
      # determine correct parameter for weights for multiple cell countries
      # (weights below zero indicate an error)
      # only determine them for cases where something has to be removed
      p        <- rep(1, nyears(area))
      names(p) <- rownames(cellWeight)

      criticalWarnings <- c("Jacobian is singular (1/condition=0.0e+00) (see allowSingular option)",
                            "Jacobian is completely unusable (all zero entries?)",
                            "Iteration limit exceeded")

      for (ti in getYears(area[, active, ])) {

        sol  <- nleqslv(rep(1, nyears(area[, ti, ])), findWeight,
                        cellArea = t(.arrayReduce(area[, ti, ])),
                        isoReduction = reduction[ti], cellWeight = cellWeight[ti, ],
                        control = list(allowSingular = TRUE))
        p[ti] <- sol[["x"]]
        msg   <- sol[["message"]]

        if (msg %in% criticalWarnings) {

          vcat(2, paste0("No solution for ", iso, ", ", landType, ", ", msg, ".",
                         "Restart from higher initial guess."))

          sol  <- nleqslv(rep(10^10, nyears(area[, ti, ])), findWeight,
                          cellArea = t(.arrayReduce(area[, ti, ])),
                          isoReduction = reduction[ti], cellWeight = cellWeight[ti, ],
                          control = list(allowSingular = TRUE))
          p[ti] <- sol[["x"]]
          msg   <- sol[["message"]]
          if (msg %in% criticalWarnings) warning("No solution for ", iso, ", ", landType, ", ", msg, ".")

        }
      }

      if (any(p[active] < 0)) vcat(1, "Negative weight of p=", p, " for: ", landType, " ", iso, " ", active)
      removedActive <- area * (1 - (1 - as.magpie(cellWeight, spatial = 2))^as.magpie(p))
      removed[, active, ] <- removedActive[, active, ]
    }
    return(removed)
  }

  # loop over countries
  countries <- getItems(lu, dim = "iso")
  relocated <- list()
  for (iso in countries) {

    relocated[[iso]] <- lu[iso, , ]
    allocate <- setNames(relocated[[iso]][, , 1] * 0, NULL)

    vegCIso <- vegC[iso, , ]

    # normalized vegetation carbon (with small correction to ensure values between [0,1))
    vegCN <- t(.arrayReduce(vegCIso / (as.magpie(apply(vegCIso, 2, max)) + 10^-10)))

    ###########################
    ### Reduction procedure ###
    ###########################

    # loop over all land use categories, that have to be reallocated
    for (landType in nature) {

      catReduce <- .arrayReduce(reduce[iso, , landType])

      # check if area has to be cleared
      if (any(catReduce != 0)) {
        # for other land cell with highest vegc and for all forest categories lowest vegc should be cleared first
        if (landType == "other") {
          cellWeight <- vegCN
        } else {
          cellWeight <- (1 - 10^-16 - vegCN)
        }

        if (landType == "other" && !is.null(potShr)) {
          # other land removed here becomes forest: take it first from the part of other land that
          # lies within the forest potential headroom (potential forest area minus existing forest)
          # and only the remainder from other land outside the potential
          headroom <- setNames(potShr[iso, , ] * luCellArea[iso, , ]
                               - dimSums(lu[iso, , forests], dim = 3), NULL)
          headroom[headroom < 0] <- 0
          catArea <- setNames(relocated[[iso]][, , landType], NULL)
          excess  <- catArea - headroom
          excess[excess < 0] <- 0
          inPot    <- catArea - excess
          reduceIn <- pmax(catReduce, -.arrayReduce(dimSums(inPot, dim = 1)))
          reduceOut <- catReduce - reduceIn
          shortfall <- round(-reduceOut, 4)
          if (any(shortfall > 0)) {
            vcat(2, "Forest potential too small in ", iso, ": ",
                 paste(names(shortfall)[shortfall > 0], shortfall[shortfall > 0], "Mha", collapse = ", "),
                 " of other land converted to forest outside the potential")
          }
          removed <- (.removeArea(inPot, reduceIn, cellWeight, landType)
                      + .removeArea(excess, reduceOut, cellWeight, landType))
        } else {
          removed <- .removeArea(relocated[[iso]][, , landType], catReduce, cellWeight, landType)
        }

        # remove area from cells and put to "allocate" area
        relocated[[iso]][, , landType] <- relocated[[iso]][, , landType] - removed
        allocate <- allocate + removed
      }
    }

    ############################
    ### Allocation procedure ###
    ############################

    # relocate other land to areas with low vegetation carbon density
    # check if other land has to be filled
    catIncrease <- .arrayReduce(increase[iso, , "other"])
    if (any(catIncrease != 0)) {
      # move area from "allocate" area to other land
      added <- .removeArea(allocate, -catIncrease, 1 - 10^-16 - vegCN, "other (increase)")
      relocated[[iso]][, , "other"] <- relocated[[iso]][, , "other"] + added
      allocate <- allocate - added
    }

    # relocate forest land to remaining "allocate" area
    # check if forests has to be filled
    catIncrease <- increase[iso, , forests]
    if (any(catIncrease != 0)) {
      # move area from "allocate" area to forests
      forestsShare <- catIncrease / (setNames(dimSums(catIncrease, dim = 3), NULL) + 10^-10)
      relocated[[iso]][, , forests] <- (relocated[[iso]][, , forests] + setCells(forestsShare, "GLO") * allocate)
      allocate[, , ] <- 0
    }

    ############################
    ### Check reallocation   ###
    ############################

    error <- abs(dimSums(relocated[[iso]][, , nature], dim = 1) - natTarget[iso, , ])
    if (max(error) >= 0.001) {
      landuse <- getItems(error, dim = 3)
      luMismatches <- paste(landuse[unique(which(error >= 0.001, arr.ind = TRUE)[, 3])], collapse = ", ")
      warning("Mismatch (", round(max(error), 3), " Mha) in ", iso, " for ", luMismatches)
    }

  }

  lu[names(relocated), , ] <- mbind(relocated)

  .checkCellArea <- function(lu, luCellArea) {
    map <- data.frame(from = getItems(lu, dim = 3), to = "sum")
    error <- abs(toolAggregate(lu, map, dim = 3) - luCellArea)
    cell <- rownames(which(error == max(error), arr.ind = TRUE))
    if (max(error) > 10e-4) {
      warning("Total cell areas differ (max diff = ", max(error), " in ", cell, ")!")
    }
  }
  .checkCellArea(lu, luCellArea)

  error <- abs(toolCountryFill(dimSums(lu[, , nature], dim = c("x", "y")),
                               fill = 0, verbosity = 2) - natTarget)
  if (max(error) > 10e-4) {
    country <- rownames(which(error == max(error), arr.ind = TRUE))
    warning("Mismatch between computed and target land use (max error = ", max(error), " in ", country, ")")
  }
  getComment(lu) <- NULL
  return(lu)
}
