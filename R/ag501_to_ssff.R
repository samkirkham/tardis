#' ag501_to_ssff
#'
#' @description Converts processed AG501 file created by \code{\link{process_ag501}} to SSFF files
#'
#' @details Takes an output file from the \code{\link{process_ag501}} function, loops over user-specified sensor_array, writes all data for a sensor to SSFF file. Also gets palate trace from a non-processed user-specified file. Writes one SSFF file per sensor channel + a file for lip aperture + a file for palate trace. The palate trace is written for each file for compatibility with EMU-webApp, where the palate trace needs to be specified for every token.
#'
#' @param filename file to process (Carstens AG501 .txt file, with header); assumes they have column names set by \code{\link{process_ag501}} and that the sampling rate of the EMA data is 250 Hz
#' @param sensor_array character array of sensor names to extract
#' @param pal_filepath filepath to speaker-specific palate trace file (currently assumes that it hasn't been through process_ag501 as it's typically not necessary to obtain derived variables for the palate trace - there assumes original Carstens column names; e.g. Ch1_X, ...)
#' @param pal_sensor number of sensor used for palate trace in pal_filepath file
#' @param output_dir directory to save output SSFF files for each sensor
#'
#' @author Sam Kirkham
#'
#' @examples
#' # To run on a single file
#' #' note the subset of sensor channels in 'sensors' - not all sensors are specified here as you often don't want all channels in the EMU database
#' \dontrun{
#' sensors <- c("TT", "TB", "TD", "LT", "UT", "UL", "LL")
#' ag501_to_ssff(filename = "sf2_0005.txt", sensor_array = sensors, pal_filepath = "sf2_palate.txt", pal_sensor = 15, output_dir = "path/to/dir")
#' }
#'
#' # To run on a directory of files using lapply (not run)
#' #' note the subset of sensor channels in 'sensors' - not all sensors are specified here as you often don't want all channels in the EMU database
#' \dontrun{
#' files <- list.files("path/to/dir", pattern = "*.txt", full.names = TRUE)
#' sensors <- c("TT", "TB", "TD", "LT", "UT", "UL", "LL")
#' lapply(files, ag501_to_ssff, sensor_array = sensors, pal_filepath = "sf2_palate.txt", pal_sensor = 15, output_dir = "path/to/dir")
#' }
#'
#' @note The wrassp library has a header size limit of 1kB, limiting the number of tracks that an SSFF file can contain when written to disk - this is a major reason why the \code{\link{ag501_to_ssff}} function writes one file per sensor, rather than writing all sensors inside one file. Note that it's the size of the header in memory that's the issue, so trackname length is more of a factor than the actual number of tracks, but you will hit the 1kB limit once you hit ~48 tracks, depending on track names. If this is a critical issue then you could fork the wrassp project and easily increase the header size in wrassp's \href{https://github.com/IPS-LMU/wrassp/blob/1411e0d4995d1f14b2c6f991e7b8194d9d8598ee/src/assp/headers.c#L2776}{headers.c} code (the link should take you to the specific line in question). Warning: the 1kB header size assumption also exists elsewhere in the wrassp library, so proceed with caution if you plan to change this!
#'
#' @export ag501_to_ssff
ag501_to_ssff <- function(filename, sensor_array, pal_filepath, pal_sensor, output_dir){
  d <- read.table(filename, header=T)
  for(i in 1:length(sensor_array)){
    output_name <-  paste0(gsub(".txt", ".", basename(filename)), sensor_array[i])
    d_sensor <- dplyr::select(d, tidyselect::starts_with(sensor_array[i]) & !dplyr::contains("extra") & !dplyr::contains("rms"))
    ado <- list()
    attr(ado, "sampleRate") <- 250
    attr(ado, "startTime") <- 0
    attr(ado, "startRecord") <- as.integer(1)
    attr(ado, "endRecord") <- as.integer(length(d_sensor[,1]))
    class(ado) <- "AsspDataObj"
    wrassp::AsspFileFormat(ado) <- "SSFF"
    wrassp::AsspDataFormat(ado) <- "binary"
    # set encoding for each track (32 = N tracks per sensor)
    attr(ado, "trackFormats") <- rep("REAL32", length(d_sensor[,1]*32))
    track_list <- colnames(d_sensor)
    # data = as.numeric(...) to handle columns that are full of NAs
    for(i in 1:length(track_list)){
      ado <- wrassp::addTrack(ado,
                              trackname = track_list[i],
                              data = as.numeric(d_sensor[[track_list[i]]]),
                              format = "REAL32")
    }
    wrassp::write.AsspDataObj(dobj = ado, file = paste0(output_dir, output_name))
    }
  pal <- get_palate_trace(pal_filepath, pal_sensor)
  pal_name <-  paste0(gsub(".txt", ".", basename(filename)), "PAL")
  wrassp::write.AsspDataObj(dobj = pal, file = paste0(output_dir, pal_name))
}
