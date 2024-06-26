#' @title toolForestRelocate
#' @description Reallocates cellular forest information from LUH2
#'              to better match FAO forest information
#'
#' @param lu        uncorrected landuse initialisation data set (cell level)
#' @param luCountry uncorrected landuse initialisation on country level
#' @param natTarget target natural land allocation on country level
#' @param vegC      vegetation carbon data used as reallocation weight
#' @return List of magpie object with results on cellular level
#' @author Kristine Karstens, Jan Philipp Dietrich, Felicitas Beier, Patrick v. Jeetze
#' @importFrom magclass setNames setItems new.magpie nyears
#' @importFrom nleqslv nleqslv
#'
#' @export

toolForestRelocate <- function(lu, luCountry, natTarget, vegC) { # nolint

  .arrayReduce <- function(x) {
    # drop dimensions but keep time dimension
    if (dim(x)[3] != 1) stop("array2D only works with a single data dimension!")
    if (dim(x)[1] == 1) return(array(x, dim = dim(x)[2], dimnames = dimnames(x)[2]))
    return(array(x, dim = dim(x)[1:2], dimnames = dimnames(x)[1:2]))
  }

  forests <- c("primforest", "secdforest", "forestry")
  nature  <- c(forests, "other")

  if (round(sum(lu) - sum(luCountry), 4) != 0) warning("lu and luCountry differ in total land area")
  if (round(sum(lu[, , nature]) - sum(natTarget), 4) != 0) warning("lu and natTarget differ in total land area")

  # store cell area to check later that it remains constant
  luCellArea <- setItems(dimSums(lu[, 1, ], dim = 3), dim = 2, NULL)

  # reduce, if necessary to FAO
  reduce <- increase <- round(natTarget - luCountry[, , nature], 8)
  reduce[reduce > 0]     <- 0
  increase[increase < 0] <- 0

  # grep land areas dependent on vegetation carbon density
  if (is.null(getYears(vegC))) getYears(vegC) <- getYears(natTarget)

  # weight function to determine correct cellweights for area removal
  findweight <- function(p, cellarea, isoreduction, cellweight) {
    rowSums(cellarea * (1 - (1 - cellweight)^p)) + isoreduction + 10^-10
  }

  # loop over countries
  countries <- getItems(lu, dim = "iso")
  l <- list()
  for (iso in countries) {

    l[[iso]] <- lu[iso, , ]
    allocate <- setNames(l[[iso]][, , 1] * 0, NULL)

    vegCIso <- vegC[iso, , ]

    # normalized vegetation carbon (with small correction to ensure values between [0,1))
    vegCN <- t(.arrayReduce(vegCIso / (as.magpie(apply(vegCIso, 2, max)) + 10^-10)))

    ###########################
    ### Reduction procedure ###
    ###########################

    # loop over all land use categories, that have to be reallocated
    for (cat in nature) {

      catreduce <- .arrayReduce(reduce[iso, , cat])

      # check if area has to be cleared
      if (any(catreduce != 0)) {
        # check for one cell countries
        if (dim(l[[iso]])[1] == 1) {
          # trivial case of one cell countries
          remove <- -as.magpie(catreduce)
        } else if (all(dimSums(l[[iso]][, , cat] != 0, dim = 1) == 1)) {
          # trivial case in which in each year exactly one cell contains land in the category to be reduced
          remove <- setNames(-1 * (l[[iso]][, , cat] != 0) * as.magpie(catreduce), NULL)
        } else {
          # for other land cell with highest vegc and for all forest categories lowest vegc should be cleared first
          if (cat == "other") {
            cellweight <- vegCN
          } else {
            cellweight <- (1 - 10^-16 - vegCN)
          }

          # check for edge case in which all land of that category must be removed and treat it separately
          fullremoval <- (round(dimSums(l[[iso]], dim = 1)[, , cat] + as.magpie(catreduce), 3) == 0)
          if (any(fullremoval)) {
            allocate[, fullremoval, ] <- (allocate[, fullremoval, ]
                                          + setNames(l[[iso]][, fullremoval, cat], NULL))
            l[[iso]][, fullremoval, cat] <- 0
            catreduce[fullremoval] <- 0
          }

          t <- (catreduce != 0)
          if (any(t)) {
            # determine correct parameter for weights for multiple cell countries
            # (weights below zero indicate an error)
            # only determine them for cases where something has to be removed
            p        <- rep(1, nyears(l[[iso]]))
            names(p) <- rownames(cellweight)

            for (ti in getYears(l[[iso]][, t, ])) {

              sol  <- nleqslv(rep(1, nyears(l[[iso]][, ti, ])), findweight,
                              cellarea = t(.arrayReduce(l[[iso]][, ti, cat])),
                              isoreduction = catreduce[ti], cellweight = cellweight[ti, ],
                              control = list(allowSingular = TRUE))
              p[ti] <- sol$x
              msg   <- sol$message
              criticalWarnings  <- c("Jacobian is singular (1/condition=0.0e+00) (see allowSingular option)",
                                     "Jacobian is completely unusable (all zero entries?)",
                                     "Iteration limit exceeded")

              if (msg %in% criticalWarnings) {

                vcat(2, paste0("No solution for ", iso, ", ", cat, ", ", msg, ".",
                               "Restart from higher intial guess."))

                sol  <- nleqslv(rep(10^10, nyears(l[[iso]][, ti, ])), findweight,
                                cellarea = t(.arrayReduce(l[[iso]][, ti, cat])),
                                isoreduction = catreduce[ti], cellweight = cellweight[ti, ],
                                control = list(allowSingular = TRUE))
                p[ti] <- sol$x
                msg   <- sol$message
                if (msg %in% criticalWarnings) warning("No solution for ", iso, ", ", cat, ", ", msg, ".")

              }
            }

            if (any(p[t] < 0)) vcat(1, "Negative weight of p=", p, " for: ", cat, " ", iso, " ", t)
            remove <- l[[iso]][, , cat] * (1 - (1 - as.magpie(cellweight, spatial = 2))^as.magpie(p))
            remove[, !t, ] <- 0
          } else {
            remove <- 0
          }
        }

        # remove area from cells and put to "allocate" area
        l[[iso]][, , cat] <- l[[iso]][, , cat] - remove
        allocate <- allocate + remove
      }
    }

    ############################
    ### Allocation procedure ###
    ############################

    catincrease <- .arrayReduce(increase[iso, , "other"])

    # relocate other land to areas with low vegetation carbon density
    # check if other land has to be filled
    if (any(catincrease != 0)) {

      t <- (catincrease != 0)

      cellweight <- (1 - 10^-16 - vegCN)

      # check for one cell countries
      if (dim(l[[iso]])[1] == 1) {
        # trivial case of one cell countries
        add <- as.magpie(catincrease)
      } else if (all(dimSums(allocate != 0, dim = 1) == 1)) {
        # trivial case in which in each year exactly one cell contains land in the category to be reduced
        add <- setNames((allocate != 0) * as.magpie(catincrease), NULL)
      } else {
        # determine correct parameter for weights for multiple cell countries (weights below zero indicate an error)

        p        <- rep(1, nyears(l[[iso]]))
        names(p) <- rownames(cellweight)

        for (ti in getYears(l[[iso]][, t, ])) {

          sol  <- nleqslv(rep(1, nyears(l[[iso]][, ti, ])), findweight,
                          cellarea = t(.arrayReduce(allocate[, ti, ])),
                          isoreduction = -catincrease[ti], cellweight = cellweight[ti, ])
          p[ti] <- sol$x
        }

        if (any(p[t] < 0)) vcat(1, "Negative weight of p=", p, " for: ", cat, " ", iso, " ", t)
        add <- allocate * (1 - (1 - as.magpie(cellweight, spatial = 2))^as.magpie(p))
      }
      add[, !t, ] <- 0

      # move area from "allocate" area to other land
      l[[iso]][, , "other"]    <- l[[iso]][, , "other"]    + add
      allocate <- allocate - add
    }

    # relocate forest land to remaining "allocate" area
    # check if forests has to be filled

    catincrease <- increase[iso, , forests]

    if (any(catincrease != 0)) {
      # move area from "allocate" area to forests
      forestsShare <- catincrease / (setNames(dimSums(catincrease, dim = 3), NULL) + 10^-10)
      l[[iso]][, , forests] <- (l[[iso]][, , forests] + setCells(forestsShare, "GLO") * allocate)
      allocate[, , ] <- 0
    }

    ############################
    ### Check reallocation   ###
    ############################

    error <- abs(dimSums(l[[iso]][, , nature], dim = 1) - natTarget[iso, , ])
    if (max(error) >= 0.001) {
      landuse <- getItems(error, dim = 3)
      luMissmatches <- paste(landuse[unique(which(error >= 0.001, arr.ind = TRUE)[, 3])], collapse = ", ")
      warning("Missmatch (", round(max(error), 3), " Mha) in ", iso, " for ", luMissmatches)
    }

  }

  lu[names(l), , ] <- mbind(l)

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
    warning("Missmatch between computed and target land use (max error = ", max(error), " in ", country, ")")
  }
  getComment(lu) <- NULL
  return(lu)
}
