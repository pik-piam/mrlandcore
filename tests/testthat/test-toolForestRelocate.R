# toy country DEU (4 cells) and one-cell country LUX, both with FAO forest above LUH forest
.toyData <- function() {
  cells <- c("10p25.50p25.DEU", "10p75.50p25.DEU", "11p25.50p25.DEU", "11p75.50p25.DEU", "6p25.49p75.LUX")
  cats  <- c("crop", "past", "range", "primforest", "secdforest", "forestry", "urban", "other")
  lu <- magclass::new.magpie(cells, "y1995", cats, fill = 0, sets = c("x.y.iso", "t", "data"))
  #                crop past range primf secdf forestry urban other
  lu[1, , ] <- c(1.0, 0.5, 0, 0.1, 0.3, 0.1, 0.1, 1.0)
  lu[2, , ] <- c(0.5, 0.5, 0, 0.2, 0.4, 0.1, 0.1, 1.2)
  lu[3, , ] <- c(0.5, 0.2, 0, 0.0, 0.2, 0.0, 0.1, 1.5)
  lu[4, , ] <- c(0.8, 0.3, 0, 0.0, 0.1, 0.0, 0.1, 0.8)
  lu[5, , ] <- c(0.2, 0.1, 0, 0.0, 0.1, 0.0, 0.0, 0.3)
  vegC <- magclass::new.magpie(cells, "y1995", "vegc", sets = c("x.y.iso", "t", "data"))
  vegC[, , ] <- c(30, 25, 80, 5, 40) # cell 3 has the highest vegC but no forest potential
  pot <- magclass::new.magpie(cells, "y1995", NULL, sets = c("x.y.iso", "t", "data"))
  pot[, , ] <- c(0.8, 0.9, 0, 0.6, 0.5)
  .twoYears <- function(x) magclass::mbind(x, magclass::setYears(x, "y2000"))
  lu <- .twoYears(lu)
  vegC <- .twoYears(vegC)
  pot <- .twoYears(pot)
  luCountry <- madrat::toolCountryFill(magclass::dimSums(lu, dim = c("x", "y")), fill = 0, verbosity = 2)
  natTarget <- luCountry[, , c("primforest", "secdforest", "forestry", "other")]
  # DEU: +1.0 Mha forest in y1995 (less than the unforested potential area of 3.0 Mha), +3.5 Mha in y2000 (more)
  natTarget["DEU", , "secdforest"] <- natTarget["DEU", , "secdforest"] + c(1.0, 3.5)
  natTarget["DEU", , "other"]      <- natTarget["DEU", , "other"] - c(1.0, 3.5)
  natTarget["LUX", , "secdforest"] <- natTarget["LUX", , "secdforest"] + 0.2
  natTarget["LUX", , "other"]      <- natTarget["LUX", , "other"] - 0.2
  return(list(lu = lu, luCountry = luCountry, natTarget = natTarget, vegC = vegC, pot = pot))
}

# toy country FRA (4 cells): FAO forest below LUH forest, so part of the removed forest becomes other land
.toyDataIncrease <- function() {
  cells <- c("2p25.47p25.FRA", "2p75.47p25.FRA", "3p25.47p25.FRA", "3p75.47p25.FRA")
  cats  <- c("crop", "past", "range", "primforest", "secdforest", "forestry", "urban", "other")
  lu <- magclass::new.magpie(cells, "y1995", cats, fill = 0, sets = c("x.y.iso", "t", "data"))
  lu[1, , ] <- c(1.0, 0.5, 0, 0.3, 0.8, 0.1, 0.1, 0.4)
  lu[2, , ] <- c(0.5, 0.5, 0, 0.0, 1.2, 0.2, 0.1, 0.2)
  lu[3, , ] <- c(0.5, 0.2, 0, 0.2, 0.6, 0.0, 0.1, 0.5)
  lu[4, , ] <- c(0.8, 0.3, 0, 0.0, 0.9, 0.1, 0.1, 0.3)
  vegC <- magclass::new.magpie(cells, "y1995", "vegc", sets = c("x.y.iso", "t", "data"))
  vegC[, , ] <- c(60, 10, 35, 20)
  .twoYears <- function(x) magclass::mbind(x, magclass::setYears(x, "y2000"))
  lu <- .twoYears(lu)
  vegC <- .twoYears(vegC)
  luCountry <- madrat::toolCountryFill(magclass::dimSums(lu, dim = c("x", "y")), fill = 0, verbosity = 2)
  natTarget <- luCountry[, , c("primforest", "secdforest", "forestry", "other")]
  # secdforest -0.8 / -1.5 Mha, of which primforest +0.3 / +0.5 and other land +0.5 / +1.0 (y1995 / y2000)
  natTarget["FRA", , "secdforest"] <- natTarget["FRA", , "secdforest"] - c(0.8, 1.5)
  natTarget["FRA", , "primforest"] <- natTarget["FRA", , "primforest"] + c(0.3, 0.5)
  natTarget["FRA", , "other"]      <- natTarget["FRA", , "other"] + c(0.5, 1.0)
  return(list(lu = lu, luCountry = luCountry, natTarget = natTarget, vegC = vegC))
}

forests <- c("primforest", "secdforest", "forestry")

test_that("potForestShr = FALSE reproduces the relocation without forest potential", {
  d <- .toyData()
  out <- suppressMessages(toolForestRelocate(d[["lu"]], d[["luCountry"]], d[["natTarget"]], d[["vegC"]],
                                             potForestShr = FALSE))
  # reference created with toolForestRelocate before the forest potential was introduced
  expect_equal(out, readRDS(test_path("toolForestRelocate_ref.rds")), tolerance = 1e-10)
})

test_that("other land increase is placed as before", {
  d <- .toyDataIncrease()
  out <- suppressMessages(toolForestRelocate(d[["lu"]], d[["luCountry"]], d[["natTarget"]], d[["vegC"]],
                                             potForestShr = FALSE))
  # reference created with toolForestRelocate before the allocation of other land used .removeArea
  expect_equal(out, readRDS(test_path("toolForestRelocate_refIncrease.rds")), tolerance = 1e-6)
})

test_that("new forest is placed within the forest potential first", {
  d <- .toyData()
  local_mocked_bindings(calcOutput = function(...) d[["pot"]])
  out <- suppressMessages(toolForestRelocate(d[["lu"]], d[["luCountry"]], d[["natTarget"]], d[["vegC"]]))

  forestIncrease <- magclass::dimSums(out[, , forests], dim = 3) - magclass::dimSums(d[["lu"]][, , forests], dim = 3)
  cellArea <- magclass::setYears(magclass::dimSums(d[["lu"]][, 1, ], dim = 3), NULL)
  potUnforested <- d[["pot"]] * cellArea - magclass::dimSums(d[["lu"]][, , forests], dim = 3)
  potUnforested[potUnforested < 0] <- 0

  # y1995: unforested potential area is large enough, no forest added outside the potential
  expect_true(all(forestIncrease[, "y1995", ] <= potUnforested[, "y1995", ] + 1e-8))
  expect_equal(as.vector(forestIncrease["11p25.50p25.DEU", "y1995", ]), 0)
  # y2000: all 3.0 Mha of unforested potential area converted, only the remaining 0.5 Mha outside
  expect_equal(as.vector(forestIncrease[1:4, "y2000", ]), c(1.0, 1.2, 0.5, 0.8), tolerance = 1e-6)
  # one-cell country
  expect_equal(as.vector(forestIncrease["6p25.49p75.LUX", , ]), c(0.2, 0.2), tolerance = 1e-8)
  # country targets met, cell areas unchanged, existing forest outside the potential kept
  total <- magclass::dimSums(out[, , c(forests, "other")], dim = c("x", "y"))
  expect_equal(total, d[["natTarget"]][c("DEU", "LUX"), , ], tolerance = 1e-6, ignore_attr = TRUE)
  expect_equal(magclass::dimSums(out, dim = 3), magclass::dimSums(d[["lu"]], dim = 3), tolerance = 1e-8)
  expect_true(all(out >= 0))
})
