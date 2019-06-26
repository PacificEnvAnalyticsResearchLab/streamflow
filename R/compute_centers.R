#' Compute the Centers of A Time-Series
#'
#' Computes the centers of a time-series using information from the resistor readings. Once the centers have been computed they can then be used for classification.
#'
#' @param data Formatted data.frame. The wide-formatted data to compute centers for.
#' @param daily_mean_resistor Character. The variable name which contains the daily mean resistor readings.
#'
#' @return A list containing the dry and wet centers.
#'
#' @examples
#' # Start by formatting the WWC data
#' formatted_wwc <- format_data(data = wwc,
#'                              date_var = "date_time",
#'                              date_start = "2015-04-01",
#'                              date_end = "2015-08-30",
#'                              site_id_var = "site_id",
#'                              sensor_type_var = "sensor_type",
#'                              resistor_level = "Resistor",
#'                              water_temp_level = "Water temp",
#'                              sensor_reading_var = "sensor_reading")
#'
#' # Then compute the centers of the wet and dry streams
#' centers <- compute_centers(data = formatted_wwc,
#'                            daily_mean_resistor = "daily_mean_resistor")
#' centers
#'
#' @export

compute_centers <- function(data, daily_mean_resistor) {

  wet <- data[data[, daily_mean_resistor] >= 0, ]
  dry <- data[data[, daily_mean_resistor] < 0, ]

  if (nrow(wet) == 0) {
    stop("No wet readings to compute centers for")
  }

  if (nrow(dry) == 0) {
    stop("No dry readings to compute centers for")
  }

  matrix_wet <- as.matrix(wet[, 4:27])
  matrix_dry <- as.matrix(dry[, 4:27])

  comp_dry <- composite_medoid(data = matrix_dry, index = F)[1, ]
  comp_wet <- composite_medoid(data = matrix_wet, index = F)[1, ]

  return(list(dry_center = comp_dry, wet_center = comp_wet))
}
