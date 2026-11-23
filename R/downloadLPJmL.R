#' @title downloadLPJmL
#' @description Download LPJmL content by version, climate model and scenario
#'
#' @param subtype Switch between different input
#'                It consists of LPJmL version, climate model,
#'                scenario and variable.
#' @return metadata entry
#' @author Kristine Karstens, Marcos Alves, Felicitas Beier
#' @examples
#' \dontrun{
#' readSource("LPJmL", subtype = "lpjml5.10.0-m4:GSWP3-W5E5:historical:pnv:soilc", convert = FALSE)
#' }
#' @importFrom utils download.file untar
#' @importFrom madrat toolSplitSubtype

downloadLPJmL <- function(subtype = "lpjml5.10.0-m4:GSWP3-W5E5:historical:pnv:soilc") { # nolint

  x     <- toolSplitSubtype(subtype,
                            list(version      = NULL,
                                 climatemodel = NULL,
                                 scenario     = NULL,
                                 runtype      = NULL,
                                 variable     = NULL))

  # To Do: Upload LPJmL data to Zenodo and include the following code here
  # build a zenodo download
  # check zenodor or ZenodoManager for easy downloading,
  # or simply use
  #     download.file(url, destfile)                            # nolint: commented_code_linter.
  #     untar(destfile)                                         # nolint: commented_code_linter.
  #     unlink/file.remove(destfile) -> should be rmoved???     # nolint: commented_code_linter.
  # if zenodo download is not available
  zenodo <- FALSE

  if (zenodo == FALSE) {
    path      <- paste0("/p/projects/landuse/LPJmL_for_MAgPIE/",  # nolint: absolute_path_linter.
                        "runs_", x$version, "/output/",
                        paste("run", tolower(x$climatemodel), x$scenario, x$runtype, sep = "_"),
                        "/", x$variable, ".tgz")
    if (file.exists(path)) {
      utils::untar(path, exdir = ".")
    } else {
      stop("Data is not available. Please check, if the variable was created
           for this version, runtype and climatescenario")
    }
  }

  # read metadata
  metaJson <- lpjmlkit::read_meta(paste0(x$variable, ".bin.json"))

  # Compose meta data
  return(list(url           = NULL,   # UPDATE
              doi           = NULL,    # UPDATE
              title         = metaJson$variable,    # UPDATE
              unit          = metaJson$unit,
              author        = list(person("Jens",      "Heinke",  email = "heinke@pik-potsdam.de"),
                                   person("Christoph", "Mueller", email = "cmueller@pik-potsdam.de")),
              version       = metaJson$source,    # UPDATE
              release_date  = NULL,    # UPDATE
              description   = NULL,    # UPDATE
              license       = "Creative Commons Attribution 4.0 International (CC BY 4.0) License",
              reference     = NULL)
  )
}
