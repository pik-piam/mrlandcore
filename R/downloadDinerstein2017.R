#' @title downloadDinerstein2017
#' @description Downloads the RESOLVE Terrestrial Ecoregions 2017 (Dinerstein et al. 2017),
#' a global map of 846 ecoregions grouped into 14 biomes.
#' @author Florian Humpenoeder
#' @seealso \code{\link{readDinerstein2017}}
#' @importFrom utils download.file unzip person bibentry
#' @importFrom withr local_options

downloadDinerstein2017 <- function() {

  local_options(timeout = 3000) # ~150 MB shapefile archive

  url <- "https://storage.googleapis.com/teow2016/Ecoregions2017.zip"
  download.file(url, "Ecoregions2017.zip", mode = "wb")
  unzip("Ecoregions2017.zip")
  unlink("Ecoregions2017.zip")

  return(list(
    title   = "RESOLVE Terrestrial Ecoregions 2017",
    url     = url,
    doi     = "10.1093/biosci/bix014",
    author  = person("Eric", "Dinerstein"),
    license = "CC BY 4.0",
    reference = bibentry(
      "Article",
      title   = "An Ecoregion-Based Approach to Protecting Half the Terrestrial Realm",
      author  = c(person("Eric", "Dinerstein"), person("David", "Olson"), person("Anup", "Joshi")),
      year    = "2017",
      journal = "BioScience",
      doi     = "10.1093/biosci/bix014"
    )
  ))
}
