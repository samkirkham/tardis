#' mfcc_to_ssff
#'
#' @description Calculates MFFCs from a WAV file and write a new SSFF file containing the MFCCs
#'
#' @details Calculates Mel Frequency Cepstral Coefficients (MFCC) for a WAV file and writes output to SSFF file, which can then be imported into an EMU database. Returns SSFF file containing three tracks: MFCCs, MFCCd (deltas) and MFCCdd (delta-deltas). Default number of coefficients is 12.
#'
#' @param filepath WAV file to process
#' @param output_dir directory to save output SSFF files for each sensor
#' @param ... additional arguments to passed to tuneR::melfcc()
#'
#' @author Sam Kirkham
#'
#' @examples
#' # To run on a single file
#' \dontrun{
#' mfcc_to_ssff(filepath = "sf2_0005.wav", output_dir = "path/to/dir")
#' }
#'
#' # To run on a directory of files using lapply
#' \dontrun{
#' files <- list.files(path = "path/to/dir", pattern = ".wav", full.names = TRUE)
#' lapply(files, mfcc_to_ssff, output_dir = "path/to/dir")
#' }
#'
#' @export mfcc_to_ssff
mfcc_to_ssff <- function(filepath, output_dir, ...){
  wav <- tuneR::readWave(filepath)
  wav_melfcc <- tuneR::melfcc(wav, ...)

  # get deltas
  wav_melfcc_d <- tuneR::deltas(wav_melfcc)
  wav_melfcc_dd <- tuneR::deltas(tuneR::deltas(wav_melfcc))

  # melfcc sampling rate
  wav_dur <- (length(wav@left) / wav@samp.rate) * 1000
  mfcc_sr <- (length(wav_melfcc[,1]) / wav_dur) * 1000

  # create empty SSFF track with correct attributes
  ado <- list() # create empty list object
  attr(ado, "sampleRate") <- mfcc_sr # MFCC sample rate
  attr(ado, "origFreq")  <- as.numeric(wav@samp.rate) # original sr
  attr(ado, "startTime") <- 0 # add start time attribute
  attr(ado, "startRecord") <- as.integer(1) # row 1
  attr(ado, "endRecord") <- as.integer(length(wav_melfcc[,1])) # end
  class(ado) <- "AsspDataObj" # set class of ado
  wrassp::AsspFileFormat(ado) <- "SSFF" # set file format to SSFF
  wrassp::AsspDataFormat(ado) <- "binary" # data format
  attr(ado, "trackFormats") <- rep("REAL32", 3) # track encoding

  # add tracks to ado object
  # ...adding the whole matrix = one track with 12 fields
  # ...could add delta + delta-delta here too and add to 'ado'
  ado <- wrassp::addTrack(ado, "mfcc", wav_melfcc, format = c("REAL32"))
  ado <- wrassp::addTrack(ado, "mfcc_d", wav_melfcc_d, format = c("REAL32"))
  ado <- wrassp::addTrack(ado, "mfcc_dd", wav_melfcc_dd, format = c("REAL32"))

  # name file [basename].MFCC
  output_name <- paste0(gsub(".wav", ".", basename(filepath)), "MFCC")

  # write to file (in current directory)
  wrassp::write.AsspDataObj(dobj = ado, file = paste0(output_dir, output_name))
}
