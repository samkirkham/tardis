#' process_ag501
#'
#' @description Process Carstens AG501 .txt and return in-memory object
#'
#' @details The function takes a Carstens AG501 ASCII .txt file (with headers) and a character list of sensors corresponding to each channel in the AG501 .txt files. It then processes this file by adding user-specified column names from 'sensor_array', calculates lip aperture and various velocity measures, and returns an in-memory object (default) or writes a file to disk (via `write_file==TRUE`). For each sensor channel it returns the following variables for the x/y/z/phi/theta coordinates: TT* (unsmoothed), _filt (smoothed), _V (velocity), _V_abs (absolute velocity), _A (acceleration), _A_abs (absolute acceleration). Also returns xz_V and xyz_V (absolute tangential velocity; in xz and xyz dimensions). There is the option to omit some columns from the above processing steps, but still retain them in the output object, which can be set using the argument `exclude_list`.
#'
#' @param filename file to process (Carstens AG501 ASCII .txt file, with header)
#' @param sensor_array character array of sensor names corresponding with the channels in the .txt file (this must contain a name for each channel and names should be in sequential order). The only assumption is that this contains two channels called "LL" and "UL", which are used for calculating lip aperture.
#' @param exclude_list Character vector. After columns are re-named according to `sensor_array`, any columns containing the strings passed to this argument will be left out of the signal processing steps and then appended un-modified to the returned object (default is NULL)
#' @param sampling_rate sampling rate in Hz (default=250)
#' @param filter_type Order of low-pass Butterworth smoothing filter (default is "low")
#' @param filter_order Order of low-pass Butterworth smoothing filter (default is 5)
#' @param filter_cutoffs Cut-off frequency of low-pass Butterworth smoothing filter in Hz (default is 20). If `filter_type="pass"` then `filter_cutoffs` must given 2 values.
#' @param write_file Writes object as a .txt file file to `output_directory` (default is FALSE, which returns the data as an in-memory object).
#' @param output_directory Directory in which to save output file when `write_file=TRUE` (default is NULL). If `write_file=TRUE` and `output_directory=NULL` then the file will be written to the current directory.
#'
#' @author Sam Kirkham
#'
#' @examples
#' # To run on a single file
#' \dontrun{
#' sensors <- c("TT", "TB", "TD", "LT", "UT", "UL", "LL", "U1", "B1", "B2", "B3", "LH", "RH", "NO", "PA", "U2")
#' process_ag501(filename = "inst/extdata/ema_original/sf2_0005.txt", sensor_array = sensors, sampling_rate=250, filter_type="low", filter_order=5, filter_cutoffs=20)
#' # exclude any columns from processing that containing the strings in 'exclude_list'. These will be present in the returned object, but will not be filtered or undergo any further calculations
#' process_ag501(filename = "inst/extdata/ema_original/sf2_0005.txt", sensor_array = sensors, sampling_rate=250, filter_type="low", filter_order=5, filter_cutoffs=20, exclude_list =  c("U1", "U2", "B1", "B2", "B3", "LH", "RH", "NO", "PA", "UT", "phi", "theta", "rms", "extra"))
#' }
#'
#' @importFrom magrittr %>%
#' @export process_ag501

process_ag501 <- function(filename, sensor_array, exclude_list=NULL, sampling_rate=250, filter_type="low", filter_order=5, filter_cutoffs=20, write_file=FALSE, output_directory=NULL){

  # read file; col names; calculate lip aperture ----------------------------

  # read file
  d <- utils::read.table(filename, header=T)
  output_name <- basename(filename)

  # make column names (Ch1_X) lower case (ch1_x) for easier processing
  # e.g. this allows us to more easily change ch1_x into TTx
  colnames(d) <- tolower(colnames(d))

  # rename columns (e.g. from "ch1_x" to "TTx", etc)
  for(i in 1:length(colnames(d))){
    d <- dplyr::rename_with(d,
                            ~ sub(paste0("ch",i,"_"), sensor_array[i], .x),
                            dplyr::starts_with(paste0("ch",i,"_"))
                            )
    }

  # calculate lip aperture for LAz, LAxz, LAxyz
  d$LAz <- sqrt((d$ULz - d$LLz)^2)
  d$LAxz <- sqrt((d$ULx - d$LLx)^2 + (d$ULz - d$LLz)^2)
  d$LAxyz <-sqrt((d$ULx - d$LLx)^2 + (d$ULy - d$LLy)^2 + (d$ULz - d$LLz)^2)



  # filter channels and calculate velocity---------------------------------------------------------------------

  # list of cols to run processing on
  toprocess <- dplyr::select(d, !tidyselect::contains(exclude_list))

  # list of cols to not run processing on (but will still be returned in output)
  donotprocess <- dplyr::select(d, tidyselect::contains(exclude_list))

  # run all of the following on 'toprocess'
  toprocess_out <- toprocess %>%

    # make all cols numeric (i.e. relevant for those containing NAs)
    dplyr::mutate(
      dplyr::across(
        .cols=tidyselect::all_of(colnames(toprocess)),
        .fns=as.numeric)) %>%

    # butterworth filter
    dplyr::mutate(
      dplyr::across(
        .cols=tidyselect::all_of(colnames(toprocess)),
        .fns=tadaR::butterworth_filter, filter_type=filter_type, filter_order=filter_order, filter_cutoffs=filter_cutoffs, sampling_rate=sampling_rate,
        .names="{.col}_filt")) %>%

    # velocity (central difference)
    dplyr::mutate(
      dplyr::across(
        #.cols=paste0(tidyselect::all_of(colnames(toprocess)), "_filt"), # adds '_filt' suffix to cols
        .cols=tidyselect::contains("_filt"), # adds '_filt' suffix to cols
        .fns=tadaR::central_difference, sampling_rate=sampling_rate, abs=FALSE,
        .names="{.col}_V")) %>%
    dplyr::rename_with(~ gsub("filt_V", "V", .x), tidyselect::contains("filt_V")) %>% # rename to _Vel

    # absolute velocity
    dplyr::mutate(
      dplyr::across(
        .cols=tidyselect::ends_with("_V"),
        .fns=base::abs,
        .names="{.col}_abs")) %>%

    # acceleration (central difference) (put sampling rate as argument)
    dplyr::mutate(
      dplyr::across(
        .cols=tidyselect::ends_with("_V"),
        .fns=tadaR::central_difference, sampling_rate=sampling_rate, abs=FALSE,
        .names="{.col}A")) %>%
    dplyr::rename_with(~ gsub("VA", "A", .x), tidyselect::contains("_VA")) %>% # rename to _acc

    # absolute accelerataion
    dplyr::mutate(
      dplyr::across(
        .cols=tidyselect::ends_with("_A"),
        .fns=base::abs,
        .names="{.col}_abs"))
  # current pipe ends here: object saved as 'toprocess_out'

  # tangential velocity

  # list of non-LA sensor columns (e.g. TT, TB, TD, etc) for calculating tangential vel.
  tvel_cols <- colnames(toprocess)[colnames(toprocess) != c("LAz", "LAxz", "LAxyz")]
  tvel_cols <- gsub("x", "", tvel_cols)
  tvel_cols <- gsub("y", "", tvel_cols)
  tvel_cols <- gsub("z", "", tvel_cols)
  tvel_cols <- unique(tvel_cols)

  # tangential velocity ('xz_V' and 'xyz_V') on 'tvel_cols'
  for(i in 1:length(tvel_cols)){
    toprocess_out[[paste0(tvel_cols[i], "xz_V")]] = sqrt(
      (toprocess_out[[paste0(tvel_cols[i], "x_V_abs")]])^2 +
        (toprocess_out[[paste0(tvel_cols[i], "z_V_abs")]])^2
    )
    toprocess_out[[paste0(tvel_cols[i], "xyz_V")]] =  sqrt(
      (toprocess_out[[paste0(tvel_cols[i], "x_V_abs")]])^2 +
        (toprocess_out[[paste0(tvel_cols[i], "y_V_abs")]])^2 +
        (toprocess_out[[paste0(tvel_cols[i], "z_V_abs")]])^2
    )
  }

  # bind processed data with columns that weren't processed
  d_processed <- dplyr::bind_cols(toprocess_out, donotprocess)

  # if write_file==TRUE:write .txt file to output_directory
  # note: if output_directory is NULL then it will write to current directory
  if(write_file==TRUE){
    utils::write.table(d_processed,
                file = paste0(output_directory, "/", output_name),
                row.names = FALSE)
  }else
    {
      # if write_file==FALSE: return d_processed as in-memory object
      return(d_processed)
    }
}
