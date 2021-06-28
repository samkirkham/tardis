#' process_ag501
#'
#' @description Process Carstens AG501 .txt and write processed data to new .txt file
#'
#' @details The function takes a Carstens AG501 ASCII .txt file (with headers) and a character list of sensors corresponding to each channel in the AG501 .txt files. It then processes this file by adding user-specified column names from 'sensor_array', calculates lip aperture and various velocity measures, and saves an output file with same name to a new directory. For each sensor channel it returns the following variables for the x/y/z/phi/theta coordinates: _raw (unsmoothed), _filt (smoothed), _Vel (velocity), _VelAbs (absolute velocity), _Acc (acceleration), _AccAbs (absolute acceleration), _TVAbs (absolute tangential velocity; in xz and xyz dimensions).
#'
#' @param filename file to process (Carstens AG501 ASCII .txt file, with header)
#' @param sensor_array character array of sensor names corresponding with the channels in the .txt file (this must contain a name for each channel and names should be in sequential order)
#' @param sr sampling rate in Hz (default=250)
#' @param filter_order Order of low-pass Butterworth smoothing filter (default=5)
#' @param filter_cutoffs Cut-off frequency of low-pass Butterworth smoothing filter in Hz (default=20)
#' @param output_dir directory to save output SSFF files for each sensor
#'
#' @author Sam Kirkham
#'
#' @examples
#' # To run on a single file
#' \dontrun{
#' sensors <- c("TT", "TB", "TD", "LT", "UT", "UL", "LL", "U1", "B1", "B2", "B3", "LH", "RH", "NO", "PA", "U2")
#' process_ag501(filename = "sf2_0005.txt", sensor_array = sensors, output_dir = "path/to/dir")
#' }
#'
#' # To run on a directory of files using lapply
#' \dontrun{
#' files <- list.files("path/to/dir", pattern = "*.txt", full.names = TRUE)
#' sensors <- c("TT", "TB", "TD", "LT", "UT", "UL", "LL", "U1", "B1", "B2", "B3", "LH", "RH", "NO", "PA", "U2")
#' lapply(files, process_ag501, sensor_array = sensors, output_dir = "path/to/dir")
#' }
#'
#' @export process_ag501
process_ag501 <- function(filename, sensor_array, sr=250, filter_order=5, filter_cutoffs=20, output_dir){
  d <- read.table(filename, header=T)
  output_name <- basename(filename)
  colnames(d) <- tolower(colnames(d)) # make lower case (TTx vs TTX)
  for(i in 1:length(colnames(d))){
    d <- dplyr::rename_at(d,
                          dplyr::vars(dplyr::starts_with(paste0("ch",i,"_"))),
                          dplyr::funs(sub(paste0("ch",i,"_"), sensor_array[i], .))
    )
  }
  # calculate lip aperture for LAz, LAxz, LAxyz
  d$LAz <- sqrt((d$ULz - d$LLz)^2)
  d$LAxz <- sqrt((d$ULx - d$LLx)^2 + (d$ULz - d$LLz)^2)
  d$LAxyz <-sqrt((d$ULx - d$LLx)^2 + (d$ULy - d$LLy)^2 + (d$ULz - d$LLz)^2)

  # smooth all columns w/ 5th order Butterworth 20 Hz low-pass filter
  # ...this is **essential** for velocity/acceleration calculations
  # ...rename d_filt later to avoid adding _filt_ to all subsequent colnames
  d_filt <- as.data.frame(apply(d, 2, ag501::Butterworth, samplingRate = 250, order = filter_order, cutoffs = filter_cutoffs, type = "low")) # as.data.frame to match 'd' class

  # get velocity
  d_vel <- as.data.frame(apply(d_filt, 2, ag501::CentralDifference, n = 1, order = 1, samplingRate = sr))
  colnames(d_vel) <- paste(colnames(d_vel), "Vel", sep = "_")

  # get absolute-valued velocity
  d_vel_abs <- as.data.frame(apply(d_vel, 2, abs))
  colnames(d_vel_abs) <- paste(colnames(d_vel_abs), "Abs", sep = "")

  # get acceleration
  d_acc <- as.data.frame(apply(d_filt, 2, ag501::CentralDifference, n = 1, order = 2, samplingRate = sr))
  colnames(d_acc) <- paste(colnames(d_acc), "Acc", sep = "_")

  # get absolute-valued acceleration
  d_acc_abs <- as.data.frame(apply(d_acc, 2, abs))
  colnames(d_acc_abs) <- paste(colnames(d_acc_abs), "Abs", sep = "")

  # calculate tangential velocity for all sensors except LA
  # (i) subset d_vel_abs, but without LA variables as these are already derived Euclidean distance measures, so you can't calculate them again
  d_vel_abs_subset <- dplyr::select(d_vel_abs, !starts_with("LA"))

  # (ii) add 'xz_TVAbs' and 'xyz_TVAbs' to 'd_vel_abs_subset'
  for(i in 1:length(sensor_array)){
    d_vel_abs_subset[[paste0(sensor_array[i], "xz_TVAbs")]] = sqrt(
      (d_vel_abs_subset[[paste0(sensor_array[i], "x_VelAbs")]])^2 +
        (d_vel_abs_subset[[paste0(sensor_array[i], "z_VelAbs")]])^2
    )
    d_vel_abs_subset[[paste0(sensor_array[i], "xyz_TVAbs")]] =  sqrt(
      (d_vel_abs_subset[[paste0(sensor_array[i], "x_VelAbs")]])^2 +
        (d_vel_abs_subset[[paste0(sensor_array[i], "y_VelAbs")]])^2 +
        (d_vel_abs_subset[[paste0(sensor_array[i], "z_VelAbs")]])^2
    )
  }

  # (iii) select all columns containing name "TVAbs" and save to new object
  TVAbs <- dplyr::select(d_vel_abs_subset, contains("TVAbs"))

  # rename 'd' (=raw) and filt' object here
  colnames(d) <- paste(colnames(d), "raw", sep = "_") # add colnames
  colnames(d_filt) <- paste(colnames(d_filt), "filt", sep = "_") # add colnames

  # add smoothed, velocity and acceleration data together
  dat <- cbind(d, d_filt, d_vel, d_acc, d_vel_abs, d_acc_abs, TVAbs)

  # write to file
  write.table(dat,
              file = paste0(output_dir, output_name),
              row.names = FALSE)
}
