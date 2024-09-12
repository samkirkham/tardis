#' get_palate_trace
#'
#' @description Converts a AG501 palate trace file to Assp Data Object (note: this function is typically not used alone and is instead called by \code{\link{process_ag501}})
#'
#' @details Takes a Carstens AG501 .txt file (with headers) corresponding to a speaker-specific palate trace and returns a SSFF object. This function is not designed to run alone and is instead called from \code{\link{process_ag501}}
#'
#' @param pal_filepath file to process Carstens AG501 .txt file, with header containing original header names) and that the sampling rate of the EMA data is 250 Hz. Note: do not use \code{\link{process_ag501}} on the palate trace file to be used as input to this function.
#' @param pal_sensor number of sensor used for palate trace
#'
#' @author Sam Kirkham
#'
#' @note At present, this assumes that the input is an unprocessed palate trace file, with the original column names (i.e. the file has not been processed via process_ag501). This is because process_ag501 on such files is unnecessary given that we only need to obtain X/Y/Z coordinates and do not require smoothing and derived variables for the palate trace
get_palate_trace <- function(pal_filepath, pal_sensor){
  p <- utils::read.table(pal_filepath, header=T)
  pal <- list()
  attr(pal, "sampleRate") <- 250
  attr(pal, "startTime") <- 0
  attr(pal, "startRecord") <- as.integer(1)
  attr(pal, "endRecord") <- as.integer(length(p[,1]))
  class(pal) <- "AsspDataObj"
  wrassp::AsspFileFormat(pal) <- "SSFF"
  wrassp::AsspDataFormat(pal) <- "binary"
  attr(pal, "trackFormats") <- rep("REAL32", 3) # 3 = x/y/z
  # add tracks for palate trace in XYZ dimensions
  pal = wrassp::addTrack(pal, trackname = "PALx",
                         data = as.numeric(p[[paste0("Ch", pal_sensor, "_X")]]),
                         format = "REAL32")
  pal = wrassp::addTrack(pal, trackname = "PALy",
                         data = as.numeric(p[[paste0("Ch", pal_sensor, "_Y")]]),
                         format = "REAL32")
  pal = wrassp::addTrack(pal, trackname = "PALz",
                         data = as.numeric(p[[paste0("Ch", pal_sensor, "_Z")]]),
                         format = "REAL32")
}
