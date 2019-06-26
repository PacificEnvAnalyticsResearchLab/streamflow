#' Format data for classification
#'
#' Takes raw data and formats it for use in the classification functions.
#'
#' @param data Unformatted data object.
#' @param date_var Character. The name of the date variable. Should be a POSIX (datetime) variable.
#' @param date_start,date_end Character. The start and end dates to use for classification.
#' @param site_id_var Character. The name of the variable which contains the site ID information. Site IDs should be unique for each sensor.
#' @param sensor_type_var Character. The variable in the data which states the type of sensor, whether it be the resistor readings or the water temperature reading.
#' @param resistor_level Character. The name of the resistor sensor in the sensor_type_var argument.
#' @param water_temp_level Character. The name of the water temperature sensor in the sensor_type_var argument.
#'
#' @return Returns a wide-format data object which can be used in the classification functions.
#'
#' @examples
#' # The wwc data is packaged along with the streamflow package.
#' format_data(data = wwc,
#'             date_var = "date_time",
#'             date_start = "2015-04-01",
#'             date_end = "2015-08-30",
#'             site_id_var = "site_id",
#'             sensor_type_var = "sensor_type",
#'             resistor_level = "Resistor",
#'             water_temp_level = "Water temp",
#'             sensor_reading_var = "sensor_reading")
#'
#' @export

format_data <- function(data, date_var, date_start, date_end, site_id_var,
                        sensor_type_var, resistor_level, water_temp_level, sensor_reading_var) {

  if (!missing(date_start)) {
    data <- data[as.Date(data[, date_var]) >= as.Date(date_start), ]
  }

  if (!missing(date_end)) {
    data <- data[as.Date(data[, date_var]) <= as.Date(date_end), ]
  }

  mean_resistor <- data[data[, sensor_type_var] == resistor_level, ]
  mean_resistor <- dplyr::group_by(mean_resistor, !!rlang::sym(site_id_var), date = as.Date(!!rlang::sym(date_var)))
  mean_resistor <- dplyr::summarize(mean_resistor, daily_mean_resistor = mean(!!rlang::sym(sensor_reading_var), na.rm = TRUE))

  data <- data[data[, sensor_type_var] == water_temp_level, ]
  data <- dplyr::group_by(data, !!rlang::sym(site_id_var), date = as.Date(!!rlang::sym(date_var)), hour = lubridate::hour(!!rlang::sym(date_var)))
  data <- dplyr::summarise(data, !!rlang::sym(sensor_reading_var))
  data <- tidyr::spread(data, hour, !!rlang::sym(sensor_reading_var), sep = "_")
  data <- dplyr::ungroup(data)

  data <- dplyr::left_join(data, mean_resistor, by = c(site_id_var, "date"))

  data <- dplyr::select(data, site_id_var, date, daily_mean_resistor, dplyr::everything())
  data <- data[complete.cases(data), ]

  return(data)
}
