#' @title downloadGAEZv4
#' @description Download sub-set of GAEZ version 4 data
#' @param subtype Name of file to be downloaded
#'                Available subtypes are for example:
#'                "mci_CRUTS32_Hist_0010.tif" (irrigated multiple cropping suitability for years 2000-2010),
#'                "mcr_CRUTS32_Hist_0010.tif" (rainfed multiple cropping suitability for years 2000-2010)
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' readSource("GAEZv4", convert = "onlycorrect")
#' }
#'
#' @importFrom utils download.file tail

downloadGAEZv4 <- function(subtype = NULL) {

  if (grepl("mci_CRUTS32_Hist_0010\\.tif|mcr_CRUTS32_Hist_0010\\.tif", subtype)) {

    # setting main download link for historical CRU data:
    mainlink <- "https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/res01/CRUTS32/Hist"

    # download specific subtype from main download link
    download.file(file.path(mainlink, subtype), destfile = subtype, mode = "wb")

    # store meta data
    metadata <- list(url_data_viewer = "https://gaez.fao.org/pages/data-viewer-theme-2",
                     url = mainlink,
                     type = subtype,
                     author = "Food and Agriculture Organization of the United Nations (FAO)",
                     title = "GAEZ (Global Agro-ecological Zones) Data Portal",
                     description = paste0("Multi-cropping class (",
                                          ifelse(grepl("mci", subtype), "irrigated", "rainfed"),
                                          ") for the time period 2000-2010
                                          using climate data source CRUTS32 based on Historical"),
                     license = "Creative Commons Attribution 4.0 International (CC BY 4.0)",
                     unit = "1",
                     legend = list("0" = "0",
                                   "1" = "no cropping",
                                   "2" = "single cropping",
                                   "3" = "limited double cropping",
                                   "4" = "double cropping",
                                   "5" = "double cropping with rice",
                                   "6" = "double rice cropping",
                                   "7" = "tripple cropping",
                                   "8" = "tripple rice cropping"))

  } else {
    warning("Currently available subtypes are: `mci_CRUTS32_Hist_0010.tif`
            and `mcr_CRUTS32_Hist_0010.tif`.
            Other subtypes require updating the downloadGAEZv4 function.")
    metadata <- "Data not available"

    # Note: Different subtypes may require updating the main link above!

    # More available data/subtypes can be found at https://gaez.fao.org/pages/data-viewer-theme-2
    # when navigating through the data-viewer-theme-2,
    # e.g., selecting "2 - Agro-climatic Resources" on top, then choosing open in full screen,
    # then under choose renderer select "Choose multi-cropping class", under "Sub-Theme Name is" choose "empty",
    # under "Variable Name is" choose "multi-cropping class (rain-fed)", under "Time Period is" choose: 2000-2010,
    # under "Climate Data Source is" choose: CRUTS32
    # Finally, when clicking on little arrow on the bottom to be able to download the data
    # The last step will show the download link for your chosen data.

  }

  # return data
  return(metadata)
}
